#include <assert.h>
#include <dirent.h>
#include <fcntl.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>


const char* get_file_type(const char* path) {
    struct stat st;
    lstat(path, &st);
    if (S_ISLNK(st.st_mode)) {
        return "symbolic link";
    } else if (S_ISDIR(st.st_mode)) {
        return "directory";
    } else if (S_ISCHR(st.st_mode)) {
        return "character device";
    } else if (S_ISBLK(st.st_mode)) {
        return "block device";
    } else if (S_ISFIFO(st.st_mode)) {
        return "fifo";
    } else if (S_ISSOCK(st.st_mode)) {
        return "socket";
    } else if (S_ISREG(st.st_mode)) {
        return "regular file";
    } else {
        assert(0);
    }
}
int scan(char* dir_path, int level = 0) {
    if (level > 10) {
        abort();
    }


    char entry_path[PATH_MAX + 1];
    strncpy(entry_path, dir_path, sizeof(entry_path));
    size_t path_len = strlen(dir_path);

    if (entry_path[path_len - 1] != '/') {
        entry_path[path_len]     = '/';
        entry_path[path_len + 1] = '\0';
        ++path_len;
    }

    DIR*           dir = opendir(dir_path);
    struct dirent* entry;
    while ((entry = readdir(dir)) != NULL) {
        const char* type;
        strncpy(
            entry_path + path_len,
            entry->d_name,
            sizeof(entry_path) - path_len);
        if (entry_path[path_len] != '.') {
            type = get_file_type(entry_path);
            struct stat st;
            lstat(entry_path, &st);
            long int size = (intmax_t)st.st_size;
            if (S_ISDIR(st.st_mode)) {
                printf(
                    "%-18s| %-18ld|  %.*s%s\n",
                    type,
                    size,
                    level * 2,
                    "                                     ",
                    entry_path);
                scan(entry_path, level + 1);
            } else {
                printf(
                    "%-18s| %-18ld| %.*s%s\n",
                    type,
                    size,
                    level * 2,
                    "                                     ",
                    entry_path);
            }
        }
    }
    closedir(dir);
}

int main(int argc, char* argv[]) {
    printf("%-18s| %-18s| %s\n", "TYPE", "SIZE(Byte)", "PATH");

    if (argc >= 2) {
        scan(argv[1]);
    } else {
        scan(".");
    }


    return 0;
};
