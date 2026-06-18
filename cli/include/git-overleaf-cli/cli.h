#ifndef GIT_OVERLEAF_CLI_H
#define GIT_OVERLEAF_CLI_H

#include <stddef.h>

#define GO_DEFAULT_URL "https://www.overleaf.com"
#define GO_DEFAULT_COOKIE_FILE "~/.git-overleaf-cookies"
#define GO_BASE_REF "refs/git-overleaf/base"
#define GO_SYNC_METADATA_FILE ".git-overleaf-sync.json"

typedef struct {
    char message[2048];
} GoError;

typedef struct {
    char *data;
    size_t len;
} GoBuffer;

typedef struct {
    char *url;
    char *cookie;
    char *cookie_file;
    char *git;
    char *unzip;
    int url_explicit;
    int json;
    int verbose;
} GoConfig;

typedef struct {
    int status;
    char *output;
} GoProcessResult;

typedef struct {
    char *id;
    char *name;
    char *owner_email;
} GoProject;

typedef struct {
    GoProject *items;
    size_t len;
} GoProjectList;

typedef struct {
    char *temp_dir;
    char *root;
} GoSnapshot;

int go_error(GoError *err, const char *fmt, ...);
char *go_xstrdup(const char *s);
char *go_xstrndup(const char *s, size_t n);
char *go_trim(char *s);
char *go_trimmed_dup(const char *s);
char *go_expand_home(const char *path);
char *go_path_join(const char *left, const char *right);
char *go_url_join(const char *base, const char *path);
char *go_sanitize_url(const char *url);
int go_write_private_file(const char *path, const char *text, GoError *err);
int go_read_file(const char *path, char **out, GoError *err);
void go_buffer_free(GoBuffer *buffer);

void go_config_init(GoConfig *cfg);
void go_config_free(GoConfig *cfg);
int go_config_load_cookie(GoConfig *cfg, GoError *err);

int go_process_run(char *const argv[], const char *cwd, char *const env[],
                   int allow_failure, GoProcessResult *out, GoError *err);
void go_process_result_free(GoProcessResult *result);

int go_git_capture(const GoConfig *cfg, const char *repo, const char *const args[],
                   size_t argc, char *const env[], int allow_failure,
                   GoProcessResult *out, GoError *err);
int go_git_output(const GoConfig *cfg, const char *repo, const char *const args[],
                  size_t argc, char *const env[], char **out, GoError *err);
int go_git_ok(const GoConfig *cfg, const char *repo, const char *const args[],
              size_t argc, char *const env[], GoError *err);
int go_git_config_get(const GoConfig *cfg, const char *repo, const char *key,
                      char **out, GoError *err);
int go_git_config_set(const GoConfig *cfg, const char *repo, const char *key,
                      const char *value, GoError *err);
int go_git_root(const GoConfig *cfg, const char *directory, char **out, GoError *err);
int go_git_current_branch(const GoConfig *cfg, const char *repo, char **out, GoError *err);
int go_git_rev_parse(const GoConfig *cfg, const char *repo, const char *revision,
                     char **out, GoError *err);
int go_git_rev_parse_verify(const GoConfig *cfg, const char *repo, const char *revision,
                            char **out, GoError *err);
int go_git_tree_id(const GoConfig *cfg, const char *repo, const char *revision,
                   char **out, GoError *err);
int go_git_is_clean(const GoConfig *cfg, const char *repo, GoError *err);
int go_git_is_ancestor(const GoConfig *cfg, const char *repo, const char *ancestor,
                       const char *descendant, int *is_ancestor, GoError *err);
int go_git_commit_directory(const GoConfig *cfg, const char *repo, const char *directory,
                            const char *parent, const char *message,
                            char **commit_out, GoError *err);
int go_git_write_metadata(const GoConfig *cfg, const char *repo, const char *project_id,
                          const char *project_name, GoError *err);
int go_git_set_base_ref(const GoConfig *cfg, const char *repo, const char *revision,
                        GoError *err);
int go_git_prepare_sync_metadata_repo(const char *repo, GoError *err);

int go_ensure_directory(const char *path, GoError *err);
int go_directory_empty_or_missing(const char *path, int *empty, GoError *err);
int go_copy_tree(const char *source, const char *destination, GoError *err);
int go_remove_tree(const char *path, GoError *err);
int go_make_temp_dir(char **out, GoError *err);
int go_make_temp_file(char **out, GoError *err);
int go_normalize_extracted_root(const char *directory, char **out, GoError *err);
int go_delete_sync_metadata(const char *root, char **metadata_text, GoError *err);

int go_http_get(const GoConfig *cfg, const char *url, const char *referer,
                GoBuffer *out, GoError *err);
int go_http_download(const GoConfig *cfg, const char *url, const char *referer,
                     const char *output_file, GoError *err);

int go_overleaf_list_projects(const GoConfig *cfg, GoProjectList *out, GoError *err);
void go_project_list_free(GoProjectList *list);
int go_overleaf_download_snapshot(const GoConfig *cfg, const char *project_id,
                                  GoSnapshot *out, GoError *err);
void go_snapshot_free(GoSnapshot *snapshot);
int go_overleaf_clone(const GoConfig *cfg, const char *project_id,
                      const char *project_name, const char *target,
                      GoError *err);
int go_overleaf_init(const GoConfig *cfg, const char *repo, const char *project_id,
                     const char *project_name, GoError *err);
int go_overleaf_pull(const GoConfig *cfg, const char *repo_arg, GoError *err);

#endif
