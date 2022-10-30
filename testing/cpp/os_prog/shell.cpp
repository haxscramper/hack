#include <stdlib.h>
#include <iostream>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>

// Simplifed xv6 shell.

#define MAXARGS 10

// All commands have at least a type. Have looked at the type, the code
// typically casts the *cmd to some specific cmd type.
struct Cmd
{
    int type; //  ' ' (exec), | (pipe), '<' or '>' for redirection
};

struct Execcmd
{
    int   type;          /// ' '
    char* argv[MAXARGS]; /// arguments to the command to be exec-ed
};

struct Redircmd
{
    int   type;  /// < or >
    Cmd*  cmd;   /// the command to be run (e.g., an execcmd)
    char* file;  /// the input/output file
    int   flags; /// flags for open() indicating read or write
    int   fd;    /// the file descriptor number to use for the file
};

struct Pipecmd
{
    int  type;  /// |
    Cmd* left;  /// left side of pipe
    Cmd* right; /// right side of pipe
};

int  fork1(void); // Fork but exits on failure.
Cmd* parsecmd(char*);

// Execute cmd.  Never returns.
void runcmd(Cmd* cmd) {
    int       p[2], r;
    Execcmd*  ecmd;
    Pipecmd*  pcmd;
    Redircmd* rcmd;

    if (cmd == 0) {
        _exit(0);
    }
    switch (cmd->type) {
        default: fprintf(stderr, "unknown runcmd\n"); _exit(-1);

        case ' ': {
            ecmd = (Execcmd*)cmd;
            if (ecmd->argv[0] == 0) {
                _exit(0);
            }
            std::cout << "> executing command " << ecmd->argv[0] << "\n";
            int code = fork();
            if (code < 0) {
                std::cerr << "Subprocess fork failed";

            } else if (code == 0) {
                // code = 0 is returned to the child process

                // Call `exec`, replacing the current process with
                execvp(ecmd->argv[0], ecmd->argv);
            } else {
                // parent process get the child id
                wait(nullptr);
            }
            break;
        }

        case '>':
        case '<': {
            rcmd = (Redircmd*)cmd;
            fprintf(stderr, "redir not implemented\n");
            // Your code here ...
            runcmd(rcmd->cmd);
            break;
        }
        case '|':
            pcmd = (Pipecmd*)cmd;
            fprintf(stderr, "pipe not implemented\n");
            // Your code here ...
            break;
    }
    _exit(0);
}

int getcmd(char* buf, int nbuf) {
    if (isatty(fileno(stdin))) {
        fprintf(stdout, "cs134$ ");
    }
    memset(buf, 0, nbuf);
    if (fgets(buf, nbuf, stdin) == 0) {
        return -1; // EOF
    }
    return 0;
}

int main(void) {
    static char buf[100];
    int         fd, r;

    // Read and run input commands.
    while (getcmd(buf, sizeof(buf)) >= 0) {
        if (buf[0] == 'c' && buf[1] == 'd' && buf[2] == ' ') {
            // Clumsy but will have to do for now.
            // Chdir has no effect on the parent if run in the child.
            buf[strlen(buf) - 1] = 0; // chop \n
            if (chdir(buf + 3) < 0) {
                fprintf(stderr, "cannot cd %s\n", buf + 3);
            }
            continue;
        }
        if (fork1() == 0) {
            runcmd(parsecmd(buf));
        }
        wait(&r);
    }
    exit(0);
}

int fork1(void) {
    int pid;

    pid = fork();
    if (pid == -1) {
        perror("fork");
    }
    return pid;
}

Cmd* execcmd(void) {
    Execcmd* cmd = new Execcmd();
    cmd->type    = ' ';
    return (Cmd*)cmd;
}

Cmd* redircmd(Cmd* subcmd, char* file, int type) {
    Redircmd* cmd = new Redircmd();
    cmd->type     = type;
    cmd->cmd      = subcmd;
    cmd->file     = file;
    cmd->flags    = (type == '<') ? O_RDONLY : O_WRONLY | O_CREAT | O_TRUNC;
    cmd->fd       = (type == '<') ? 0 : 1;
    return (Cmd*)cmd;
}

Cmd* pipecmd(Cmd* left, Cmd* right) {
    Pipecmd* cmd = new Pipecmd();
    memset(cmd, 0, sizeof(*cmd));
    cmd->type  = '|';
    cmd->left  = left;
    cmd->right = right;
    return (Cmd*)cmd;
}

// Parsing

char whitespace[] = " \t\r\n\v";
char symbols[]    = "<|>";

int gettoken(char** ps, char* es, char** q, char** eq) {
    char* s;
    int   ret;

    s = *ps;
    while (s < es && strchr(whitespace, *s)) {
        s++;
    }
    if (q) {
        *q = s;
    }
    ret = *s;
    switch (*s) {
        case 0: break;
        case '|':
        case '<': s++; break;
        case '>': s++; break;
        default:
            ret = 'a';
            while (s < es && !strchr(whitespace, *s) && !strchr(symbols, *s)) {
                s++;
            }
            break;
    }
    if (eq) {
        *eq = s;
    }
    while (s < es && strchr(whitespace, *s)) {
        s++;
    }
    *ps = s;
    return ret;
}

int peek(char** ps, char* es, char const* toks) {
    char* s;

    s = *ps;
    while (s < es && strchr(whitespace, *s)) {
        s++;
    }
    *ps = s;
    return *s && strchr(toks, *s);
}

Cmd* parseline(char**, char*);
Cmd* parsepipe(char**, char*);
Cmd* parseexec(char**, char*);

/// make a copy of the characters in the input buffer, starting from s
/// through es. null-terminate the copy to make it a string.
char* mkcopy(char* s, char* es) {
    int   n = es - s;
    char* c = (char*)malloc(n + 1);
    assert(c);
    strncpy(c, s, n);
    c[n] = 0;
    return c;
}

Cmd* parsecmd(char* s) {
    char* es  = s + strlen(s);
    Cmd*  cmd = parseline(&s, es);
    peek(&s, es, "");
    if (s != es) {
        fprintf(stderr, "leftovers: %s\n", s);
        exit(-1);
    }
    return cmd;
}

Cmd* parseline(char** ps, char* es) {
    Cmd* cmd;
    cmd = parsepipe(ps, es);
    return cmd;
}

Cmd* parsepipe(char** ps, char* es) {
    Cmd* cmd = parseexec(ps, es);
    if (peek(ps, es, "|")) {
        gettoken(ps, es, 0, 0);
        cmd = pipecmd(cmd, parsepipe(ps, es));
    }
    return cmd;
}

Cmd* parseredirs(Cmd* cmd, char** ps, char* es) {
    int   tok;
    char *q, *eq;

    while (peek(ps, es, "<>")) {
        tok = gettoken(ps, es, 0, 0);
        if (gettoken(ps, es, &q, &eq) != 'a') {
            fprintf(stderr, "missing file for redirection\n");
            exit(-1);
        }
        switch (tok) {
            case '<': cmd = redircmd(cmd, mkcopy(q, eq), '<'); break;
            case '>': cmd = redircmd(cmd, mkcopy(q, eq), '>'); break;
        }
    }
    return cmd;
}

Cmd* parseexec(char** ps, char* es) {
    char *   q, *eq;
    int      tok, argc;
    Execcmd* cmd;
    Cmd*     ret;

    ret = execcmd();
    cmd = (Execcmd*)ret;

    argc = 0;
    ret  = parseredirs(ret, ps, es);
    while (!peek(ps, es, "|")) {
        if ((tok = gettoken(ps, es, &q, &eq)) == 0) {
            break;
        }
        if (tok != 'a') {
            fprintf(stderr, "syntax error\n");
            exit(-1);
        }
        cmd->argv[argc] = mkcopy(q, eq);
        argc++;
        if (argc >= MAXARGS) {
            fprintf(stderr, "too many args\n");
            exit(-1);
        }
        ret = parseredirs(ret, ps, es);
    }
    cmd->argv[argc] = 0;
    return ret;
}
