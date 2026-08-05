#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

void __KG__exit(int code) {
	exit(code);
}

int __KG_POSIX__execute(const char* program, const char* args) {
    int pid = fork();

    if (pid == 0) {
        execlp(program, program, args, NULL);
        exit(1);
    }

    return pid;
}

int __KG_POSIX__wait_process(int pid) {
    int status;

    waitpid(pid, &status, 0);

    return WEXITSTATUS(status);
}
