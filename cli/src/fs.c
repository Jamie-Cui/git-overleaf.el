#define _POSIX_C_SOURCE 200809L

#include "git-overleaf-cli/cli.h"

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

int git_overleaf_ensure_directory(const char *path, GoError *err) {
  if (!path || !*path) {
    return git_overleaf_error(err, "empty directory path");
  }
  char *copy = git_overleaf_xstrdup(path);
  if (!copy) {
    return git_overleaf_error(err, "out of memory");
  }
  for (char *p = copy + 1; *p; p++) {
    if (*p == '/') {
      *p = '\0';
      if (mkdir(copy, 0777) != 0 && errno != EEXIST) {
        int saved = errno;
        free(copy);
        return git_overleaf_error(err, "could not create directory %s: %s",
                                  path, strerror(saved));
      }
      *p = '/';
    }
  }
  if (mkdir(copy, 0777) != 0 && errno != EEXIST) {
    int saved = errno;
    free(copy);
    return git_overleaf_error(err, "could not create directory %s: %s", path,
                              strerror(saved));
  }
  free(copy);
  return 0;
}

int git_overleaf_directory_empty_or_missing(const char *path, int *empty,
                                            GoError *err) {
  *empty = 1;
  struct stat st;
  if (lstat(path, &st) != 0) {
    if (errno == ENOENT) {
      return 0;
    }
    return git_overleaf_error(err, "could not inspect %s: %s", path,
                              strerror(errno));
  }
  if (!S_ISDIR(st.st_mode)) {
    *empty = 0;
    return 0;
  }
  DIR *dir = opendir(path);
  if (!dir) {
    return git_overleaf_error(err, "could not open %s: %s", path,
                              strerror(errno));
  }
  struct dirent *entry;
  while ((entry = readdir(dir))) {
    if (strcmp(entry->d_name, ".") != 0 && strcmp(entry->d_name, "..") != 0) {
      *empty = 0;
      break;
    }
  }
  closedir(dir);
  return 0;
}

static int copy_file(const char *source, const char *destination, mode_t mode,
                     GoError *err) {
  int in = open(source, O_RDONLY);
  if (in < 0) {
    return git_overleaf_error(err, "could not open %s: %s", source,
                              strerror(errno));
  }
  int out = open(destination, O_WRONLY | O_CREAT | O_TRUNC, mode & 0777);
  if (out < 0) {
    int saved = errno;
    close(in);
    return git_overleaf_error(err, "could not create %s: %s", destination,
                              strerror(saved));
  }
  char buffer[65536];
  for (;;) {
    ssize_t n = read(in, buffer, sizeof(buffer));
    if (n < 0) {
      if (errno == EINTR) {
        continue;
      }
      int saved = errno;
      close(in);
      close(out);
      return git_overleaf_error(err, "could not read %s: %s", source,
                                strerror(saved));
    }
    if (n == 0) {
      break;
    }
    char *p = buffer;
    ssize_t remaining = n;
    while (remaining > 0) {
      ssize_t written = write(out, p, (size_t)remaining);
      if (written < 0) {
        if (errno == EINTR) {
          continue;
        }
        int saved = errno;
        close(in);
        close(out);
        return git_overleaf_error(err, "could not write %s: %s", destination,
                                  strerror(saved));
      }
      p += written;
      remaining -= written;
    }
  }
  if (close(in) != 0 || close(out) != 0) {
    return git_overleaf_error(err, "could not finish copying %s", source);
  }
  return 0;
}

int git_overleaf_copy_tree(const char *source, const char *destination,
                           GoError *err) {
  struct stat st;
  if (lstat(source, &st) != 0) {
    return git_overleaf_error(err, "could not inspect %s: %s", source,
                              strerror(errno));
  }
  if (S_ISDIR(st.st_mode)) {
    if (git_overleaf_ensure_directory(destination, err) != 0) {
      return -1;
    }
    DIR *dir = opendir(source);
    if (!dir) {
      return git_overleaf_error(err, "could not open %s: %s", source,
                                strerror(errno));
    }
    struct dirent *entry;
    while ((entry = readdir(dir))) {
      if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
        continue;
      }
      char *child_source = git_overleaf_path_join(source, entry->d_name);
      char *child_dest = git_overleaf_path_join(destination, entry->d_name);
      if (!child_source || !child_dest) {
        free(child_source);
        free(child_dest);
        closedir(dir);
        return git_overleaf_error(err, "out of memory");
      }
      int rc = git_overleaf_copy_tree(child_source, child_dest, err);
      free(child_source);
      free(child_dest);
      if (rc != 0) {
        closedir(dir);
        return -1;
      }
    }
    closedir(dir);
    chmod(destination, st.st_mode & 0777);
    return 0;
  }
  if (S_ISREG(st.st_mode)) {
    return copy_file(source, destination, st.st_mode, err);
  }
  return 0;
}

int git_overleaf_remove_tree(const char *path, GoError *err) {
  struct stat st;
  if (lstat(path, &st) != 0) {
    if (errno == ENOENT) {
      return 0;
    }
    return git_overleaf_error(err, "could not inspect %s: %s", path,
                              strerror(errno));
  }
  if (S_ISDIR(st.st_mode)) {
    DIR *dir = opendir(path);
    if (!dir) {
      return git_overleaf_error(err, "could not open %s: %s", path,
                                strerror(errno));
    }
    struct dirent *entry;
    while ((entry = readdir(dir))) {
      if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
        continue;
      }
      char *child = git_overleaf_path_join(path, entry->d_name);
      if (!child) {
        closedir(dir);
        return git_overleaf_error(err, "out of memory");
      }
      int rc = git_overleaf_remove_tree(child, err);
      free(child);
      if (rc != 0) {
        closedir(dir);
        return -1;
      }
    }
    closedir(dir);
    if (rmdir(path) != 0) {
      return git_overleaf_error(err, "could not remove %s: %s", path,
                                strerror(errno));
    }
  } else if (unlink(path) != 0) {
    return git_overleaf_error(err, "could not remove %s: %s", path,
                              strerror(errno));
  }
  return 0;
}

int git_overleaf_make_temp_dir(char **out, GoError *err) {
  *out = NULL;
  const char *tmp = getenv("TMPDIR");
  if (!tmp || !*tmp) {
    tmp = "/tmp";
  }
  char *template_path = git_overleaf_path_join(tmp, "git-overleaf-cli.XXXXXX");
  if (!template_path) {
    return git_overleaf_error(err, "out of memory");
  }
  if (!mkdtemp(template_path)) {
    int saved = errno;
    free(template_path);
    return git_overleaf_error(err, "could not create temporary directory: %s",
                              strerror(saved));
  }
  *out = template_path;
  return 0;
}

int git_overleaf_make_temp_file(char **out, GoError *err) {
  *out = NULL;
  const char *tmp = getenv("TMPDIR");
  if (!tmp || !*tmp) {
    tmp = "/tmp";
  }
  char *template_path = git_overleaf_path_join(tmp, "git-overleaf-cli.XXXXXX");
  if (!template_path) {
    return git_overleaf_error(err, "out of memory");
  }
  int fd = mkstemp(template_path);
  if (fd < 0) {
    int saved = errno;
    free(template_path);
    return git_overleaf_error(err, "could not create temporary file: %s",
                              strerror(saved));
  }
  close(fd);
  *out = template_path;
  return 0;
}

int git_overleaf_normalize_extracted_root(const char *directory, char **out,
                                          GoError *err) {
  *out = NULL;
  DIR *dir = opendir(directory);
  if (!dir) {
    return git_overleaf_error(err, "could not open extracted directory %s: %s",
                              directory, strerror(errno));
  }
  char *only = NULL;
  size_t count = 0;
  struct dirent *entry;
  while ((entry = readdir(dir))) {
    if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
      continue;
    }
    count++;
    free(only);
    only = git_overleaf_path_join(directory, entry->d_name);
    if (!only) {
      closedir(dir);
      return git_overleaf_error(err, "out of memory");
    }
  }
  closedir(dir);
  if (count == 1) {
    struct stat st;
    if (lstat(only, &st) == 0 && S_ISDIR(st.st_mode)) {
      *out = only;
      return 0;
    }
  }
  free(only);
  *out = git_overleaf_xstrdup(directory);
  return *out ? 0 : git_overleaf_error(err, "out of memory");
}

int git_overleaf_delete_sync_metadata(const char *root, char **metadata_text,
                                      GoError *err) {
  if (metadata_text) {
    *metadata_text = NULL;
  }
  char *path = git_overleaf_path_join(root, GO_SYNC_METADATA_FILE);
  if (!path) {
    return git_overleaf_error(err, "out of memory");
  }
  struct stat st;
  if (lstat(path, &st) != 0) {
    free(path);
    return 0;
  }
  if (metadata_text && S_ISREG(st.st_mode)) {
    char *text = NULL;
    if (git_overleaf_read_file(path, &text, err) != 0) {
      free(path);
      return -1;
    }
    *metadata_text = text;
  }
  int rc =
      S_ISDIR(st.st_mode) ? git_overleaf_remove_tree(path, err) : unlink(path);
  if (rc != 0) {
    int saved = errno;
    free(path);
    return git_overleaf_error(err, "could not remove %s: %s",
                              GO_SYNC_METADATA_FILE, strerror(saved));
  }
  free(path);
  return 0;
}
