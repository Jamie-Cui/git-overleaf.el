#define _POSIX_C_SOURCE 200809L

#include "git-overleaf-cli/cli.h"

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

static char *format_command(char *const argv[]) {
  size_t len = 0;
  for (size_t i = 0; argv[i]; i++) {
    len += strlen(argv[i]) + 1;
  }
  char *text = malloc(len + 1);
  if (!text) {
    return NULL;
  }
  text[0] = '\0';
  for (size_t i = 0; argv[i]; i++) {
    if (i > 0) {
      strcat(text, " ");
    }
    strcat(text, argv[i]);
  }
  return text;
}

int git_overleaf_process_run(char *const argv[], const char *cwd,
                             char *const env[], int allow_failure,
                             GoProcessResult *out, GoError *err) {
  memset(out, 0, sizeof(*out));
  int pipefd[2];
  if (pipe(pipefd) != 0) {
    return git_overleaf_error(err, "pipe failed: %s", strerror(errno));
  }

  pid_t pid = fork();
  if (pid < 0) {
    int saved = errno;
    close(pipefd[0]);
    close(pipefd[1]);
    return git_overleaf_error(err, "fork failed: %s", strerror(saved));
  }

  if (pid == 0) {
    close(pipefd[0]);
    dup2(pipefd[1], STDOUT_FILENO);
    dup2(pipefd[1], STDERR_FILENO);
    close(pipefd[1]);
    if (cwd && chdir(cwd) != 0) {
      _exit(127);
    }
    if (env) {
      for (size_t i = 0; env[i]; i++) {
        char *eq = strchr(env[i], '=');
        if (eq) {
          size_t name_len = (size_t)(eq - env[i]);
          char *name = git_overleaf_xstrndup(env[i], name_len);
          if (!name) {
            _exit(127);
          }
          setenv(name, eq + 1, 1);
          free(name);
        }
      }
    }
    execvp(argv[0], argv);
    _exit(errno == ENOENT ? 127 : 126);
  }

  close(pipefd[1]);
  GoBuffer buffer = {0};
  char chunk[4096];
  for (;;) {
    ssize_t n = read(pipefd[0], chunk, sizeof(chunk));
    if (n < 0) {
      if (errno == EINTR) {
        continue;
      }
      int saved = errno;
      close(pipefd[0]);
      return git_overleaf_error(err, "read from child failed: %s",
                                strerror(saved));
    }
    if (n == 0) {
      break;
    }
    char *next = realloc(buffer.data, buffer.len + (size_t)n + 1);
    if (!next) {
      close(pipefd[0]);
      free(buffer.data);
      return git_overleaf_error(err, "out of memory");
    }
    buffer.data = next;
    memcpy(buffer.data + buffer.len, chunk, (size_t)n);
    buffer.len += (size_t)n;
    buffer.data[buffer.len] = '\0';
  }
  close(pipefd[0]);

  int status = 0;
  while (waitpid(pid, &status, 0) < 0) {
    if (errno != EINTR) {
      free(buffer.data);
      return git_overleaf_error(err, "waitpid failed: %s", strerror(errno));
    }
  }

  int exit_status = 1;
  if (WIFEXITED(status)) {
    exit_status = WEXITSTATUS(status);
  } else if (WIFSIGNALED(status)) {
    exit_status = 128 + WTERMSIG(status);
  }

  if (!buffer.data) {
    buffer.data = git_overleaf_xstrdup("");
  }
  char *trimmed = git_overleaf_trimmed_dup(buffer.data ? buffer.data : "");
  free(buffer.data);
  if (!trimmed) {
    return git_overleaf_error(err, "out of memory");
  }

  out->status = exit_status;
  out->output = trimmed;
  if (!allow_failure && exit_status != 0) {
    char *command = format_command(argv);
    git_overleaf_error(err, "%s failed with status %d: %s",
                       command ? command : argv[0], exit_status,
                       out->output && *out->output ? out->output : "no output");
    free(command);
    return -1;
  }
  return 0;
}

void git_overleaf_process_result_free(GoProcessResult *result) {
  if (result) {
    free(result->output);
    result->output = NULL;
    result->status = 0;
  }
}
