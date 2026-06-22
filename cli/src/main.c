#define _POSIX_C_SOURCE 200809L

#include "git-overleaf-cli/cli.h"

#include <curl/curl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void usage(FILE *stream) {
  fprintf(
      stream,
      "git-overleaf-cli 0.1 MVP\n"
      "\n"
      "Usage:\n"
      "  git-overleaf-cli [GLOBAL-OPTIONS] auth --cookie COOKIE [--cookie-file "
      "FILE]\n"
      "  git-overleaf-cli [GLOBAL-OPTIONS] list\n"
      "  git-overleaf-cli [GLOBAL-OPTIONS] clone --project-id ID "
      "[--project-name NAME] TARGET\n"
      "  git-overleaf-cli [GLOBAL-OPTIONS] init --project-id ID "
      "[--project-name NAME] [--repo DIR]\n"
      "  git-overleaf-cli [GLOBAL-OPTIONS] pull [--repo DIR]\n"
      "\n"
      "Global options:\n"
      "  --url URL            Overleaf URL (default: "
      "https://www.overleaf.com)\n"
      "  --cookie COOKIE      Raw Cookie header for this run\n"
      "  --cookie-file FILE   Cookie file (default: ~/.git-overleaf-cookies)\n"
      "  --git PATH           Git executable (default: git)\n"
      "  --unzip PATH         Unzip executable (default: unzip)\n"
      "  --help               Show this help\n"
      "\n"
      "No webdriver authentication is implemented in this MVP.\n");
}

static int need_value(int argc, char **argv, int index, GoError *err) {
  if (index + 1 >= argc) {
    return git_overleaf_error(err, "%s requires a value", argv[index]);
  }
  return 0;
}

static int parse_global(GoConfig *cfg, int argc, char **argv, int *index,
                        GoError *err) {
  while (*index < argc) {
    const char *arg = argv[*index];
    if (strcmp(arg, "--help") == 0 || strcmp(arg, "-h") == 0) {
      usage(stdout);
      exit(0);
    } else if (strcmp(arg, "--url") == 0) {
      if (need_value(argc, argv, *index, err) != 0) {
        return -1;
      }
      free(cfg->url);
      cfg->url = git_overleaf_sanitize_url(argv[++(*index)]);
      cfg->url_explicit = 1;
      if (!cfg->url) {
        return git_overleaf_error(err, "out of memory");
      }
    } else if (strcmp(arg, "--cookie") == 0) {
      if (need_value(argc, argv, *index, err) != 0) {
        return -1;
      }
      free(cfg->cookie);
      cfg->cookie = git_overleaf_trimmed_dup(argv[++(*index)]);
      if (!cfg->cookie) {
        return git_overleaf_error(err, "out of memory");
      }
    } else if (strcmp(arg, "--cookie-file") == 0) {
      if (need_value(argc, argv, *index, err) != 0) {
        return -1;
      }
      free(cfg->cookie_file);
      cfg->cookie_file = git_overleaf_xstrdup(argv[++(*index)]);
      if (!cfg->cookie_file) {
        return git_overleaf_error(err, "out of memory");
      }
    } else if (strcmp(arg, "--git") == 0) {
      if (need_value(argc, argv, *index, err) != 0) {
        return -1;
      }
      free(cfg->git);
      cfg->git = git_overleaf_xstrdup(argv[++(*index)]);
      if (!cfg->git) {
        return git_overleaf_error(err, "out of memory");
      }
    } else if (strcmp(arg, "--unzip") == 0) {
      if (need_value(argc, argv, *index, err) != 0) {
        return -1;
      }
      free(cfg->unzip);
      cfg->unzip = git_overleaf_xstrdup(argv[++(*index)]);
      if (!cfg->unzip) {
        return git_overleaf_error(err, "out of memory");
      }
    } else if (arg[0] == '-') {
      return git_overleaf_error(err, "unknown global option: %s", arg);
    } else {
      break;
    }
    (*index)++;
  }
  return 0;
}

static int command_auth(GoConfig *cfg, int argc, char **argv, GoError *err) {
  const char *cookie = cfg->cookie;
  for (int i = 0; i < argc; i++) {
    if (strcmp(argv[i], "--cookie") == 0) {
      if (need_value(argc, argv, i, err) != 0) {
        return -1;
      }
      cookie = argv[++i];
    } else if (strcmp(argv[i], "--cookie-file") == 0) {
      if (need_value(argc, argv, i, err) != 0) {
        return -1;
      }
      free(cfg->cookie_file);
      cfg->cookie_file = git_overleaf_xstrdup(argv[++i]);
      if (!cfg->cookie_file) {
        return git_overleaf_error(err, "out of memory");
      }
    } else {
      return git_overleaf_error(err, "unknown auth option: %s", argv[i]);
    }
  }
  if (!cookie || !*cookie) {
    return git_overleaf_error(err, "auth requires --cookie COOKIE");
  }
  if (git_overleaf_write_private_file(cfg->cookie_file ? cfg->cookie_file
                                                       : GO_DEFAULT_COOKIE_FILE,
                                      cookie, err) != 0) {
    return -1;
  }
  printf("Saved Overleaf cookies to %s\n",
         cfg->cookie_file ? cfg->cookie_file : GO_DEFAULT_COOKIE_FILE);
  return 0;
}

static int command_list(GoConfig *cfg, GoError *err) {
  if (git_overleaf_config_load_cookie(cfg, err) != 0) {
    return -1;
  }
  GoProjectList projects;
  if (git_overleaf_overleaf_list_projects(cfg, &projects, err) != 0) {
    return -1;
  }
  printf("%-28s  %-40s  %s\n", "PROJECT ID", "NAME", "OWNER");
  for (size_t i = 0; i < projects.len; i++) {
    printf("%-28s  %-40s  %s\n", projects.items[i].id, projects.items[i].name,
           projects.items[i].owner_email);
  }
  git_overleaf_project_list_free(&projects);
  return 0;
}

static int command_clone(GoConfig *cfg, int argc, char **argv, GoError *err) {
  const char *project_id = NULL;
  const char *project_name = NULL;
  const char *target = NULL;
  for (int i = 0; i < argc; i++) {
    if (strcmp(argv[i], "--project-id") == 0) {
      if (need_value(argc, argv, i, err) != 0) {
        return -1;
      }
      project_id = argv[++i];
    } else if (strcmp(argv[i], "--project-name") == 0) {
      if (need_value(argc, argv, i, err) != 0) {
        return -1;
      }
      project_name = argv[++i];
    } else if (argv[i][0] == '-') {
      return git_overleaf_error(err, "unknown clone option: %s", argv[i]);
    } else if (!target) {
      target = argv[i];
    } else {
      return git_overleaf_error(err, "unexpected clone argument: %s", argv[i]);
    }
  }
  if (!project_id) {
    return git_overleaf_error(err, "clone requires --project-id ID");
  }
  if (!target) {
    return git_overleaf_error(err, "clone requires TARGET");
  }
  if (git_overleaf_config_load_cookie(cfg, err) != 0) {
    return -1;
  }
  if (git_overleaf_overleaf_clone(cfg, project_id, project_name, target, err) !=
      0) {
    return -1;
  }
  printf("Cloned `%s' into %s\n", project_name ? project_name : project_id,
         target);
  return 0;
}

static int command_init(GoConfig *cfg, int argc, char **argv, GoError *err) {
  const char *project_id = NULL;
  const char *project_name = NULL;
  const char *repo = ".";
  for (int i = 0; i < argc; i++) {
    if (strcmp(argv[i], "--project-id") == 0) {
      if (need_value(argc, argv, i, err) != 0) {
        return -1;
      }
      project_id = argv[++i];
    } else if (strcmp(argv[i], "--project-name") == 0) {
      if (need_value(argc, argv, i, err) != 0) {
        return -1;
      }
      project_name = argv[++i];
    } else if (strcmp(argv[i], "--repo") == 0) {
      if (need_value(argc, argv, i, err) != 0) {
        return -1;
      }
      repo = argv[++i];
    } else {
      return git_overleaf_error(err, "unknown init option: %s", argv[i]);
    }
  }
  if (!project_id) {
    return git_overleaf_error(err, "init requires --project-id ID");
  }
  if (git_overleaf_config_load_cookie(cfg, err) != 0) {
    return -1;
  }
  if (git_overleaf_overleaf_init(cfg, repo, project_id, project_name, err) !=
      0) {
    return -1;
  }
  printf("Configured repository to track Overleaf project `%s'\n",
         project_name ? project_name : project_id);
  return 0;
}

static int command_pull(GoConfig *cfg, int argc, char **argv, GoError *err) {
  const char *repo = ".";
  for (int i = 0; i < argc; i++) {
    if (strcmp(argv[i], "--repo") == 0) {
      if (need_value(argc, argv, i, err) != 0) {
        return -1;
      }
      repo = argv[++i];
    } else {
      return git_overleaf_error(err, "unknown pull option: %s", argv[i]);
    }
  }
  if (git_overleaf_config_load_cookie(cfg, err) != 0) {
    return -1;
  }
  return git_overleaf_overleaf_pull(cfg, repo, err);
}

int main(int argc, char **argv) {
  GoConfig cfg;
  GoError err = {{0}};
  git_overleaf_config_init(&cfg);

  int index = 1;
  int rc = 1;
  if (argc == 1) {
    usage(stderr);
    git_overleaf_config_free(&cfg);
    return 2;
  }
  if (parse_global(&cfg, argc, argv, &index, &err) != 0) {
    fprintf(stderr, "git-overleaf-cli: %s\n", err.message);
    git_overleaf_config_free(&cfg);
    return 2;
  }
  if (index >= argc) {
    usage(stderr);
    git_overleaf_config_free(&cfg);
    return 2;
  }

  curl_global_init(CURL_GLOBAL_DEFAULT);
  const char *command = argv[index++];
  if (strcmp(command, "auth") == 0) {
    rc = command_auth(&cfg, argc - index, argv + index, &err);
  } else if (strcmp(command, "list") == 0) {
    rc = command_list(&cfg, &err);
  } else if (strcmp(command, "clone") == 0) {
    rc = command_clone(&cfg, argc - index, argv + index, &err);
  } else if (strcmp(command, "init") == 0) {
    rc = command_init(&cfg, argc - index, argv + index, &err);
  } else if (strcmp(command, "pull") == 0) {
    rc = command_pull(&cfg, argc - index, argv + index, &err);
  } else if (strcmp(command, "push") == 0 ||
             strcmp(command, "overwrite") == 0) {
    rc = git_overleaf_error(
        &err,
        "%s is not implemented in the MVP; use Emacs git-overleaf for now",
        command);
  } else {
    rc = git_overleaf_error(&err, "unknown command: %s", command);
  }
  curl_global_cleanup();

  if (rc != 0) {
    fprintf(stderr, "git-overleaf-cli: %s\n", err.message);
  }
  git_overleaf_config_free(&cfg);
  return rc == 0 ? 0 : 1;
}
