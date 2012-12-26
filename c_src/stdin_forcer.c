/* Original from https://github.com/mattsta/erlang-stdinout-pool */
#include <sys/wait.h>
#include <sys/types.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <netinet/in.h>

#define PARENT_READ     readpipe[0]
#define CHILD_WRITE     readpipe[1]
#define CHILD_READ      writepipe[0]
#define PARENT_WRITE    writepipe[1]

#define DUP2CLOSE(oldfd, newfd) (dup2(oldfd, newfd) == newfd && close(oldfd) == 0)

#define PACKET_N 4
#define BUFF_SIZE 4096

union packet_size_buf {
  char     char_b[PACKET_N];
  uint32_t int_b;
};

void  read_all(int fd, char * buf, size_t len);
void write_all(int fd, char * buf, size_t len);

int main(int argc, char *argv[]) {
  int readpipe[2], writepipe[2];
  pid_t cpid;

  /*fprintf(stderr, "init\n");*/

  assert(1 < argc && argc < 64);

  if (pipe(readpipe) == -1 || pipe(writepipe) == -1) {
    perror("pipe");
    exit(EXIT_FAILURE);
  }

  cpid = fork();
  if (cpid == -1) { perror("fork"); exit(EXIT_FAILURE); }

  if (cpid == 0) {
    /* Forked Child with STDIN forwarding */
    char *cmd = argv[1];
    char *exec_args[64] = {0};
    int i;
  
    for (i = 0; i < argc - 1; i++) {
      /* args to stdin_forcer are the program and optional args to exec.
         Here we copy pointers pointing to strings of cmd/args.
         exec_args is indexed one lower than argv. */
      exec_args[i] = argv[i+1];
    }
    
    close(PARENT_READ);  /* We aren't the parent. Decrement fd refcounts. */
    close(PARENT_WRITE);

    /* CHILD_READ  = STDIN  to the exec'd process.
       CHILD_WRITE = STDOUT to the exec'd process. */
    if (!DUP2CLOSE(CHILD_READ,   STDIN_FILENO) ||
        !DUP2CLOSE(CHILD_WRITE, STDOUT_FILENO)) {
      perror("dup2 or close");
      _exit(EXIT_FAILURE);
    }

    /*fprintf(stderr, "run %s\n", cmd);*/

    /* At this point, the execv'd program's STDIN and STDOUT are the pipe */
    if (execvp(cmd, exec_args) == -1) {
      perror("execvp");
    }
    _exit(EXIT_FAILURE);  /* Silence a warning */
  }
  else {
    /* Original Parent Process */
    char * buf;
    union packet_size_buf size_buf;
    uint32_t packet_len;
    int status;

    /*fprintf(stderr, "hello\n");*/

    close(CHILD_READ);  /* We aren't the child.  Close its read/write. */
    close(CHILD_WRITE);

    if(read(STDIN_FILENO, size_buf.char_b, PACKET_N) < PACKET_N) {
      perror("read input data size");
      _exit(EXIT_FAILURE);
    }

    packet_len = ntohl((uint32_t)size_buf.int_b);
    buf = malloc(packet_len);

    /* fprintf(stderr, "read packet_len %i\n", packet_len); */

    read_all(STDIN_FILENO, buf, packet_len);
    write_all(PARENT_WRITE, buf, packet_len);

    free(buf);

    close(PARENT_WRITE); /* closing PARENT_WRITE sends EOF to CHILD_READ */

    packet_len = 0;
    buf = malloc(0);

    {
      size_t t = 0;
      do{
        buf = realloc(buf, (packet_len / BUFF_SIZE + 1) * BUFF_SIZE);
        t = read(PARENT_READ, buf + packet_len, BUFF_SIZE);
        packet_len += t;
      } while(t == BUFF_SIZE);

      /*fprintf(stderr, "read packet_len %i\n", packet_len);*/
    }
    size_buf.int_b = htonl(packet_len);
    if(write(STDOUT_FILENO, size_buf.char_b, PACKET_N) < PACKET_N) {
      perror("write output data size");
      _exit(EXIT_FAILURE);
    }

    write_all(STDOUT_FILENO, buf, packet_len);
    free(buf);

    wait(&status);          /* Wait for child to exit */
    if(status) {
      fprintf(stderr, "child error status %i\n", status);
      _exit(EXIT_FAILURE);
    }

    close(PARENT_READ);      /* done reading from writepipe */
    exit(EXIT_SUCCESS);      /* This was a triumph */
  }
}

void read_all(int fd, char * buf, size_t len) {
  size_t t;
  if(len == 0) {
    return;
  }
  do {
    t = read(fd, buf, len);
    /*fprintf(stderr, "read %i byted\n", (int)t);*/
    if(t < 1) {
      perror("read input data");
      _exit(EXIT_FAILURE);
    } 
    len -= t;
    buf += t;
  } while(len);
}

void write_all(int fd, char * buf, size_t len) {
  size_t t;
  if(len == 0) {
    return;
  }
  do {
    t = write(fd, buf, len);
    /*fprintf(stderr, "write %i byted\n", (int)t);*/
    if(t < 1) {
      perror("write input data");
      _exit(EXIT_FAILURE);
    } 
    len -= t;
    buf += t;
  } while(len);
}
