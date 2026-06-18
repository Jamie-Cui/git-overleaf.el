#define _POSIX_C_SOURCE 200809L

#include "git-overleaf-cli/cli.h"

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

int go_error(GoError *err, const char *fmt, ...) {
  if (err) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(err->message, sizeof(err->message), fmt, ap);
    va_end(ap);
  }
  return -1;
}

char *go_xstrdup(const char *s) {
  if (!s) {
    return NULL;
  }
  size_t len = strlen(s);
  char *copy = malloc(len + 1);
  if (!copy) {
    return NULL;
  }
  memcpy(copy, s, len + 1);
  return copy;
}

char *go_xstrndup(const char *s, size_t n) {
  char *copy = malloc(n + 1);
  if (!copy) {
    return NULL;
  }
  memcpy(copy, s, n);
  copy[n] = '\0';
  return copy;
}

char *go_trim(char *s) {
  if (!s) {
    return NULL;
  }
  while (*s && isspace((unsigned char)*s)) {
    s++;
  }
  char *end = s + strlen(s);
  while (end > s && isspace((unsigned char)end[-1])) {
    *--end = '\0';
  }
  return s;
}

char *go_trimmed_dup(const char *s) {
  char *copy = go_xstrdup(s ? s : "");
  if (!copy) {
    return NULL;
  }
  char *trimmed = go_trim(copy);
  char *result = go_xstrdup(trimmed);
  free(copy);
  return result;
}

char *go_expand_home(const char *path) {
  if (!path) {
    return NULL;
  }
  if (path[0] != '~' || (path[1] != '\0' && path[1] != '/')) {
    return go_xstrdup(path);
  }
  const char *home = getenv("HOME");
  if (!home || !*home) {
    return go_xstrdup(path);
  }
  size_t home_len = strlen(home);
  size_t rest_len = strlen(path + 1);
  char *expanded = malloc(home_len + rest_len + 1);
  if (!expanded) {
    return NULL;
  }
  memcpy(expanded, home, home_len);
  memcpy(expanded + home_len, path + 1, rest_len + 1);
  return expanded;
}

char *go_path_join(const char *left, const char *right) {
  if (!left || !*left) {
    return go_xstrdup(right);
  }
  if (!right || !*right) {
    return go_xstrdup(left);
  }
  size_t left_len = strlen(left);
  size_t right_len = strlen(right);
  int needs_slash = left[left_len - 1] != '/';
  char *joined = malloc(left_len + (size_t)needs_slash + right_len + 1);
  if (!joined) {
    return NULL;
  }
  memcpy(joined, left, left_len);
  if (needs_slash) {
    joined[left_len++] = '/';
  }
  memcpy(joined + left_len, right, right_len + 1);
  return joined;
}

char *go_url_join(const char *base, const char *path) {
  char *sanitized = go_sanitize_url(base ? base : GO_DEFAULT_URL);
  if (!sanitized) {
    return NULL;
  }
  const char *suffix = path ? path : "";
  while (*suffix == '/') {
    suffix++;
  }
  size_t base_len = strlen(sanitized);
  size_t suffix_len = strlen(suffix);
  char *joined = malloc(base_len + 1 + suffix_len + 1);
  if (!joined) {
    free(sanitized);
    return NULL;
  }
  memcpy(joined, sanitized, base_len);
  joined[base_len] = '/';
  memcpy(joined + base_len + 1, suffix, suffix_len + 1);
  free(sanitized);
  return joined;
}

char *go_sanitize_url(const char *url) {
  char *copy = go_trimmed_dup(url ? url : GO_DEFAULT_URL);
  if (!copy) {
    return NULL;
  }
  size_t len = strlen(copy);
  while (len > 0 && copy[len - 1] == '/') {
    copy[--len] = '\0';
  }
  return copy;
}

int go_write_private_file(const char *path, const char *text, GoError *err) {
  char *expanded = go_expand_home(path);
  if (!expanded) {
    return go_error(err, "out of memory");
  }

  int fd = open(expanded, O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (fd < 0) {
    int saved = errno;
    free(expanded);
    return go_error(err, "could not open cookie file for writing: %s",
                    strerror(saved));
  }

  const char *data = text ? text : "";
  size_t len = strlen(data);
  ssize_t written = write(fd, data, len);
  if (written >= 0 && (size_t)written == len) {
    (void)write(fd, "\n", 1);
  }
  int close_status = close(fd);
  chmod(expanded, 0600);
  if (written < 0 || (size_t)written != len || close_status != 0) {
    int saved = errno;
    free(expanded);
    return go_error(err, "could not write cookie file: %s", strerror(saved));
  }
  free(expanded);
  return 0;
}

int go_read_file(const char *path, char **out, GoError *err) {
  *out = NULL;
  char *expanded = go_expand_home(path);
  if (!expanded) {
    return go_error(err, "out of memory");
  }

  FILE *file = fopen(expanded, "rb");
  if (!file) {
    int saved = errno;
    free(expanded);
    return go_error(err, "could not read %s: %s", path, strerror(saved));
  }
  if (fseek(file, 0, SEEK_END) != 0) {
    int saved = errno;
    fclose(file);
    free(expanded);
    return go_error(err, "could not seek %s: %s", path, strerror(saved));
  }
  long size = ftell(file);
  if (size < 0) {
    int saved = errno;
    fclose(file);
    free(expanded);
    return go_error(err, "could not size %s: %s", path, strerror(saved));
  }
  rewind(file);
  char *data = malloc((size_t)size + 1);
  if (!data) {
    fclose(file);
    free(expanded);
    return go_error(err, "out of memory");
  }
  size_t read_len = fread(data, 1, (size_t)size, file);
  if (read_len != (size_t)size && ferror(file)) {
    int saved = errno;
    free(data);
    fclose(file);
    free(expanded);
    return go_error(err, "could not read %s: %s", path, strerror(saved));
  }
  data[read_len] = '\0';
  fclose(file);
  free(expanded);
  *out = data;
  return 0;
}

void go_buffer_free(GoBuffer *buffer) {
  if (buffer) {
    free(buffer->data);
    buffer->data = NULL;
    buffer->len = 0;
  }
}

void go_config_init(GoConfig *cfg) {
  memset(cfg, 0, sizeof(*cfg));
  cfg->url = go_xstrdup(GO_DEFAULT_URL);
  cfg->cookie_file = go_xstrdup(GO_DEFAULT_COOKIE_FILE);
  cfg->git = go_xstrdup("git");
  cfg->unzip = go_xstrdup("unzip");
}

void go_config_free(GoConfig *cfg) {
  if (!cfg) {
    return;
  }
  free(cfg->url);
  free(cfg->cookie);
  free(cfg->cookie_file);
  free(cfg->git);
  free(cfg->unzip);
  memset(cfg, 0, sizeof(*cfg));
}

int go_config_load_cookie(GoConfig *cfg, GoError *err) {
  if (cfg->cookie && *go_trim(cfg->cookie)) {
    return 0;
  }

  const char *env_cookie = getenv("GIT_OVERLEAF_COOKIE");
  if (env_cookie && *env_cookie) {
    free(cfg->cookie);
    cfg->cookie = go_trimmed_dup(env_cookie);
    return cfg->cookie ? 0 : go_error(err, "out of memory");
  }

  const char *cookie_file =
      cfg->cookie_file ? cfg->cookie_file : GO_DEFAULT_COOKIE_FILE;
  char *text = NULL;
  if (go_read_file(cookie_file, &text, err) != 0) {
    return -1;
  }
  char *trimmed = go_trim(text);
  if (!*trimmed) {
    free(text);
    return go_error(err, "cookie file is empty: %s", cookie_file);
  }
  free(cfg->cookie);
  cfg->cookie = go_xstrdup(trimmed);
  free(text);
  return cfg->cookie ? 0 : go_error(err, "out of memory");
}
