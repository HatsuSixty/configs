#include <dirent.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

bool is_process_tmux(const char* pid)
{
    char* dst = (char*)malloc(256);
    memset(dst, 0, 256);
    strcat(dst, "/proc/");
    strcat(dst, pid);
    strcat(dst, "/cmdline");

    FILE* file = fopen(dst, "r");
    if (file == NULL) return false;
    char tmux[4];
    fread(tmux, sizeof(tmux), sizeof(tmux[0]), file);
    fclose(file);

    free(dst);

    return strncmp(tmux, "tmux", 4) == 0;
}

bool is_tmux_running()
{
    DIR* directory = opendir("/proc");

    while (directory) {
        struct dirent* dir_entry = readdir(directory);
        if (!dir_entry) break;
        if (is_process_tmux(dir_entry->d_name)) return true;
    }

    closedir(directory);

    return false;
}

bool execute_shell_command(char* command, char *const envp[])
{
    pid_t pid = fork();
    if (pid < 0) return false;

    if (pid == 0) {
        char* shell = "/bin/sh";
        char* const args[] = {shell, "-c", command, NULL};
        chdir("/home/hatsu/Home");
        execve(shell, args, envp);
    }

    waitpid(pid, NULL, 0);
    return true;
}

int main(int argc, const char** argv, char* const envp[])
{
    (void) argc;
    (void) argv;

    if (is_tmux_running()) {
        printf("[INFO] tmux is running, connecting to existing session\n");
        execute_shell_command("alacritty -e tmux a", envp);
    } else {
        printf("[INFO] tmux is not running, creating a new session\n");
        execute_shell_command("alacritty -e tmux", envp);
    }

    return 0;
}
