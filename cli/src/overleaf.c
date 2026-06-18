#define _POSIX_C_SOURCE 200809L

#include "git-overleaf-cli/cli.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

static char *project_path(const char *project_id) {
  size_t len = strlen("project//") + strlen(project_id) + 1;
  char *path = malloc(len);
  if (!path) {
    return NULL;
  }
  snprintf(path, len, "project/%s", project_id);
  return path;
}

static char *project_download_path(const char *project_id) {
  size_t len = strlen("project//download/zip") + strlen(project_id) + 1;
  char *path = malloc(len);
  if (!path) {
    return NULL;
  }
  snprintf(path, len, "project/%s/download/zip", project_id);
  return path;
}

static int git_unset_config(const GoConfig *cfg, const char *repo,
                            const char *key, GoError *err) {
  const char *args[] = {"config", "--local", "--unset-all", key};
  GoProcessResult result;
  int rc = go_git_capture(cfg, repo, args, 4, NULL, 1, &result, err);
  go_process_result_free(&result);
  return rc;
}

static int clear_pending(const GoConfig *cfg, const char *repo, GoError *err) {
  if (git_unset_config(cfg, repo, "git-overleaf.pendingRemoteCommit", err) !=
      0) {
    return -1;
  }
  if (git_unset_config(cfg, repo, "git-overleaf.pendingAction", err) != 0) {
    return -1;
  }
  return 0;
}

static int repo_project_id(const GoConfig *cfg, const char *repo, char **out,
                           GoError *err) {
  if (go_git_config_get(cfg, repo, "git-overleaf.projectId", out, err) != 0) {
    return -1;
  }
  if (!*out || !**out) {
    free(*out);
    *out = NULL;
    return go_error(err, "repository is not configured as an Overleaf project");
  }
  return 0;
}

static int apply_repo_url(const GoConfig *cfg, const char *repo,
                          GoConfig *local, char **repo_url_owner,
                          GoError *err) {
  *local = *cfg;
  *repo_url_owner = NULL;
  if (cfg->url_explicit) {
    return 0;
  }
  if (go_git_config_get(cfg, repo, "git-overleaf.url", repo_url_owner, err) !=
      0) {
    return -1;
  }
  if (*repo_url_owner && **repo_url_owner) {
    local->url = *repo_url_owner;
  }
  return 0;
}

int go_overleaf_download_snapshot(const GoConfig *cfg, const char *project_id,
                                  GoSnapshot *out, GoError *err) {
  memset(out, 0, sizeof(*out));
  char *temp_dir = NULL;
  char *zip_file = NULL;
  char *download_path = NULL;
  char *download_url = NULL;
  char *referer_path = NULL;
  char *referer_url = NULL;
  char *root = NULL;

  if (go_make_temp_dir(&temp_dir, err) != 0 ||
      go_make_temp_file(&zip_file, err) != 0) {
    free(temp_dir);
    free(zip_file);
    return -1;
  }
  download_path = project_download_path(project_id);
  referer_path = project_path(project_id);
  download_url = download_path ? go_url_join(cfg->url, download_path) : NULL;
  referer_url = referer_path ? go_url_join(cfg->url, referer_path) : NULL;
  if (!download_path || !referer_path || !download_url || !referer_url) {
    go_remove_tree(temp_dir, err);
    free(temp_dir);
    free(zip_file);
    free(download_path);
    free(download_url);
    free(referer_path);
    free(referer_url);
    return go_error(err, "out of memory");
  }

  if (go_http_download(cfg, download_url, referer_url, zip_file, err) != 0) {
    go_remove_tree(temp_dir, err);
    free(temp_dir);
    free(zip_file);
    free(download_path);
    free(download_url);
    free(referer_path);
    free(referer_url);
    return -1;
  }

  char *argv[] = {
      cfg->unzip ? cfg->unzip : "unzip", "-q", zip_file, "-d", temp_dir, NULL};
  GoProcessResult unzip_result;
  if (go_process_run(argv, NULL, NULL, 0, &unzip_result, err) != 0) {
    go_remove_tree(temp_dir, err);
    free(temp_dir);
    free(zip_file);
    free(download_path);
    free(download_url);
    free(referer_path);
    free(referer_url);
    return -1;
  }
  go_process_result_free(&unzip_result);
  unlink(zip_file);

  if (go_normalize_extracted_root(temp_dir, &root, err) != 0 ||
      go_delete_sync_metadata(root, NULL, err) != 0) {
    go_remove_tree(temp_dir, err);
    free(temp_dir);
    free(zip_file);
    free(download_path);
    free(download_url);
    free(referer_path);
    free(referer_url);
    free(root);
    return -1;
  }

  out->temp_dir = temp_dir;
  out->root = root;
  free(zip_file);
  free(download_path);
  free(download_url);
  free(referer_path);
  free(referer_url);
  return 0;
}

void go_snapshot_free(GoSnapshot *snapshot) {
  if (!snapshot) {
    return;
  }
  if (snapshot->temp_dir) {
    GoError ignored = {{0}};
    go_remove_tree(snapshot->temp_dir, &ignored);
  }
  free(snapshot->temp_dir);
  free(snapshot->root);
  snapshot->temp_dir = NULL;
  snapshot->root = NULL;
}

int go_overleaf_clone(const GoConfig *cfg, const char *project_id,
                      const char *project_name, const char *target,
                      GoError *err) {
  int empty = 0;
  if (go_directory_empty_or_missing(target, &empty, err) != 0) {
    return -1;
  }
  if (!empty) {
    return go_error(err, "target directory is not empty: %s", target);
  }

  GoSnapshot snapshot;
  if (go_overleaf_download_snapshot(cfg, project_id, &snapshot, err) != 0) {
    return -1;
  }

  int rc = 0;
  if (go_ensure_directory(target, err) != 0 ||
      go_copy_tree(snapshot.root, target, err) != 0) {
    rc = -1;
    goto done;
  }

  const char *init_args[] = {"init"};
  const char *add_args[] = {"add", "--all", "."};
  const char *commit_args[] = {"-c",
                               "user.name=Overleaf Project",
                               "-c",
                               "user.email=git-overleaf@local",
                               "commit",
                               "-m",
                               "chore: import project from Overleaf"};
  if (go_git_ok(cfg, target, init_args, 1, NULL, err) != 0 ||
      go_git_write_metadata(cfg, target, project_id,
                            project_name ? project_name : project_id,
                            err) != 0 ||
      go_git_prepare_sync_metadata_repo(target, err) != 0 ||
      go_git_ok(cfg, target, add_args, 3, NULL, err) != 0 ||
      go_git_ok(cfg, target, commit_args, 7, NULL, err) != 0 ||
      go_git_set_base_ref(cfg, target, "HEAD", err) != 0) {
    rc = -1;
    goto done;
  }

done:
  go_snapshot_free(&snapshot);
  return rc;
}

int go_overleaf_init(const GoConfig *cfg, const char *repo_arg,
                     const char *project_id, const char *project_name,
                     GoError *err) {
  char *repo = NULL;
  char *parent = NULL;
  char *commit = NULL;
  GoSnapshot snapshot;
  memset(&snapshot, 0, sizeof(snapshot));

  if (go_git_root(cfg, repo_arg ? repo_arg : ".", &repo, err) != 0) {
    return -1;
  }
  if (go_overleaf_download_snapshot(cfg, project_id, &snapshot, err) != 0) {
    free(repo);
    return -1;
  }

  if (go_git_rev_parse_verify(cfg, repo, GO_BASE_REF, &parent, err) != 0) {
    go_snapshot_free(&snapshot);
    free(repo);
    return -1;
  }
  char message[128];
  time_t now = time(NULL);
  struct tm tm_value;
  localtime_r(&now, &tm_value);
  strftime(message, sizeof(message),
           "overleaf: configured base snapshot %Y-%m-%d %H:%M:%S", &tm_value);
  if (go_git_commit_directory(cfg, repo, snapshot.root, parent, message,
                              &commit, err) != 0 ||
      go_git_write_metadata(cfg, repo, project_id,
                            project_name ? project_name : project_id,
                            err) != 0 ||
      clear_pending(cfg, repo, err) != 0 ||
      go_git_prepare_sync_metadata_repo(repo, err) != 0 ||
      go_git_set_base_ref(cfg, repo, commit, err) != 0) {
    go_snapshot_free(&snapshot);
    free(repo);
    free(parent);
    free(commit);
    return -1;
  }

  go_snapshot_free(&snapshot);
  free(repo);
  free(parent);
  free(commit);
  return 0;
}

typedef enum {
  SYNC_IN_SYNC,
  SYNC_HEAD_MATCHES_REMOTE,
  SYNC_REMOTE_MATCHES_BASE,
  SYNC_HEAD_MATCHES_BASE,
  SYNC_DIVERGED
} SyncState;

static SyncState classify(const char *base_tree, const char *head_tree,
                          const char *remote_tree) {
  if (strcmp(head_tree, base_tree) == 0 &&
      strcmp(remote_tree, base_tree) == 0) {
    return SYNC_IN_SYNC;
  }
  if (strcmp(head_tree, remote_tree) == 0) {
    return SYNC_HEAD_MATCHES_REMOTE;
  }
  if (strcmp(remote_tree, base_tree) == 0) {
    return SYNC_REMOTE_MATCHES_BASE;
  }
  if (strcmp(head_tree, base_tree) == 0) {
    return SYNC_HEAD_MATCHES_BASE;
  }
  return SYNC_DIVERGED;
}

int go_overleaf_pull(const GoConfig *cfg, const char *repo_arg, GoError *err) {
  char *repo = NULL;
  char *repo_url = NULL;
  char *project_id = NULL;
  char *project_name = NULL;
  char *branch = NULL;
  char *pending = NULL;
  char *base = NULL;
  char *head = NULL;
  char *remote_commit = NULL;
  char *base_tree = NULL;
  char *head_tree = NULL;
  char *remote_tree = NULL;
  GoSnapshot snapshot;
  memset(&snapshot, 0, sizeof(snapshot));

  if (go_git_root(cfg, repo_arg ? repo_arg : ".", &repo, err) != 0 ||
      repo_project_id(cfg, repo, &project_id, err) != 0 ||
      go_git_config_get(cfg, repo, "git-overleaf.projectName", &project_name,
                        err) != 0 ||
      go_git_current_branch(cfg, repo, &branch, err) != 0 ||
      go_git_is_clean(cfg, repo, err) != 0 ||
      go_git_config_get(cfg, repo, "git-overleaf.pendingAction", &pending,
                        err) != 0) {
    goto fail;
  }
  if (pending && *pending) {
    go_error(err, "unresolved pending Overleaf %s exists; finish it first",
             pending);
    goto fail;
  }

  GoConfig local_cfg;
  if (apply_repo_url(cfg, repo, &local_cfg, &repo_url, err) != 0) {
    goto fail;
  }
  if (go_overleaf_download_snapshot(&local_cfg, project_id, &snapshot, err) !=
          0 ||
      go_git_rev_parse(cfg, repo, GO_BASE_REF, &base, err) != 0 ||
      go_git_rev_parse(cfg, repo, "HEAD", &head, err) != 0) {
    goto fail;
  }

  char message[128];
  time_t now = time(NULL);
  struct tm tm_value;
  localtime_r(&now, &tm_value);
  strftime(message, sizeof(message),
           "overleaf: remote snapshot %Y-%m-%d %H:%M:%S", &tm_value);
  if (go_git_commit_directory(cfg, repo, snapshot.root, base, message,
                              &remote_commit, err) != 0 ||
      go_git_tree_id(cfg, repo, base, &base_tree, err) != 0 ||
      go_git_tree_id(cfg, repo, head, &head_tree, err) != 0 ||
      go_git_tree_id(cfg, repo, remote_commit, &remote_tree, err) != 0) {
    goto fail;
  }

  switch (classify(base_tree, head_tree, remote_tree)) {
  case SYNC_IN_SYNC:
    printf("Project `%s' is already in sync\n",
           project_name && *project_name ? project_name : project_id);
    break;
  case SYNC_HEAD_MATCHES_REMOTE:
    if (go_git_set_base_ref(cfg, repo, head, err) != 0) {
      goto fail;
    }
    printf("Local and remote content match; base ref updated\n");
    break;
  case SYNC_REMOTE_MATCHES_BASE:
    printf("No remote Overleaf changes to pull into `%s'\n", branch);
    break;
  case SYNC_HEAD_MATCHES_BASE: {
    const char *merge_args[] = {"merge", "--ff-only", remote_commit};
    if (go_git_ok(cfg, repo, merge_args, 3, NULL, err) != 0 ||
        go_git_set_base_ref(cfg, repo, "HEAD", err) != 0) {
      goto fail;
    }
    printf("Pulled remote Overleaf changes into `%s'\n", branch);
    break;
  }
  case SYNC_DIVERGED: {
    const char *merge_args[] = {"merge", "--no-ff", "--no-edit", remote_commit};
    GoProcessResult merge_result;
    if (go_git_capture(cfg, repo, merge_args, 4, NULL, 1, &merge_result, err) !=
        0) {
      goto fail;
    }
    if (merge_result.status == 0) {
      go_process_result_free(&merge_result);
      if (go_git_set_base_ref(cfg, repo, remote_commit, err) != 0) {
        goto fail;
      }
      printf("Pulled Overleaf changes into `%s'\n", branch);
    } else {
      go_process_result_free(&merge_result);
      if (go_git_config_set(cfg, repo, "git-overleaf.pendingRemoteCommit",
                            remote_commit, err) != 0 ||
          go_git_config_set(cfg, repo, "git-overleaf.pendingAction", "pull",
                            err) != 0) {
        goto fail;
      }
      printf("Merge conflict on `%s'. Resolve conflicts, commit, then push "
             "with Emacs git-overleaf or a future CLI push.\n",
             branch);
    }
    break;
  }
  }

  go_snapshot_free(&snapshot);
  free(repo);
  free(repo_url);
  free(project_id);
  free(project_name);
  free(branch);
  free(pending);
  free(base);
  free(head);
  free(remote_commit);
  free(base_tree);
  free(head_tree);
  free(remote_tree);
  return 0;

fail:
  go_snapshot_free(&snapshot);
  free(repo);
  free(repo_url);
  free(project_id);
  free(project_name);
  free(branch);
  free(pending);
  free(base);
  free(head);
  free(remote_commit);
  free(base_tree);
  free(head_tree);
  free(remote_tree);
  return -1;
}
