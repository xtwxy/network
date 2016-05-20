#include <sys/types.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <limits.h>
#include <errno.h>
#include <unistd.h>
#include <cstring>
#include <cstdlib>
#include <cstdio>

void fork_test(pid_t* pids, std::size_t size) {
  if(size == 0) {
    return;
  }
  pids[0] = getpid();
  for(std::size_t i = 1; i != size; ++i) {
    pid_t pid = fork();
    if(pid == 0) {
      printf("Child, pid = %d\n", getpid());
      //pids[i] = getpid();
      break;
    } else if(pid > 0) {
      printf("Parent, pid = %d\n", getpid());
      // parent
      pids[i] = pid;
    } else {
      // error
      printf("Error, pid = %d\n", getpid());
      exit(-1);
    }
  }
}

int main(int argc, char* argv[])
{
  if(argc != 2) {
    printf("Usage:\n%s <number of processes>\n", argv[0]);
    exit(-1);
  }

  errno = 0;
  int nop = strtol(argv[1], NULL, 10);
  if ((errno == ERANGE && (nop == LONG_MAX || nop == LONG_MIN))
      || (errno != 0 && nop == 0)) {
    perror("strtol");
    exit(EXIT_FAILURE);
  }
  printf("nop = %d\n", nop);

  pid_t* pids = (pid_t*)malloc(nop * sizeof(pid_t));

  memset(pids, 0, nop * sizeof(pid_t));

  fork_test(pids, nop);

  int status;
  if(pids[0] == getpid()) {
    for(std::size_t i = 1; i != nop; ++i) {
      printf("Wait for process to exit: pid[%d] = %d\n", i, pids[i]);
      pid_t w = waitpid(pids[i], &status, WUNTRACED | WCONTINUED);
      if (w == -1) {
        perror("waitpid");
        exit(EXIT_FAILURE);
      }

      if (WIFEXITED(status)) {
        printf("exited, status=%d\n", WEXITSTATUS(status));
      } else if (WIFSIGNALED(status)) {
        printf("killed by signal %d\n", WTERMSIG(status));
      } else if (WIFSTOPPED(status)) {
        printf("stopped by signal %d\n", WSTOPSIG(status));
      } else if (WIFCONTINUED(status)) {
        printf("continued\n");
      }

    }
    free(pids);
  }

  return EXIT_SUCCESS;
}

