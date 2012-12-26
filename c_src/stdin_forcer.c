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
    char small_buf;
    char * buf;
    union packet_size_buf size_buf;
    uint32_t packet_len;

    /*fprintf(stderr, "hello\n");*/

    close(CHILD_READ);  /* We aren't the child.  Close its read/write. */
    close(CHILD_WRITE);

    if(read(STDIN_FILENO, size_buf.char_b, PACKET_N) < PACKET_N) {
      perror("read input data size");
      _exit(EXIT_FAILURE);
    }
    
    packet_len = ntohl((uint32_t)size_buf.int_b);
    buf = malloc(packet_len);

    /*fprintf(stderr, "read packet_len %i\n", packet_len);*/

    if(read(STDIN_FILENO, buf, packet_len) < packet_len) {
      perror("read input data");
      _exit(EXIT_FAILURE);
    }
    if(write(PARENT_WRITE, buf, packet_len) < packet_len) {
      perror("write input data");
      _exit(EXIT_FAILURE);
    }
    /*fprintf(stderr, "input data %s\n", buf);*/
    free(buf);

    close(PARENT_WRITE); /* closing PARENT_WRITE sends EOF to CHILD_READ */
    wait(NULL);          /* Wait for child to exit */

    packet_len = 0;
    buf = malloc(BUFF_SIZE);

    while (read(PARENT_READ, &small_buf, 1) == 1) {
      /*fprintf(stderr, "output data %c\n", small_buf);*/
      ++packet_len;
      if(packet_len % BUFF_SIZE == 0) {
        buf = realloc(buf, (packet_len / BUFF_SIZE + 1) * BUFF_SIZE);
      }
      buf[packet_len - 1] = small_buf;
    }
    /*fprintf(stderr, "read packet_len %i\n", packet_len);*/

    size_buf.int_b = htonl(packet_len);
    if(write(STDOUT_FILENO, size_buf.char_b, PACKET_N) < PACKET_N) {
      perror("write output data size");
      _exit(EXIT_FAILURE);
    }

    if(write(STDOUT_FILENO, buf, packet_len) < packet_len) {
      perror("write output data");
      _exit(EXIT_FAILURE);
    }
    free(buf);

    close(PARENT_READ);      /* done reading from writepipe */
    exit(EXIT_SUCCESS);      /* This was a triumph */
  }
}
