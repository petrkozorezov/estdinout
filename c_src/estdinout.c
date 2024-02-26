/* Original from https://github.com/mattsta/erlang-stdinout-pool */
#include <sys/wait.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <netinet/in.h>
#include <errno.h>
#include <sys/ioctl.h>

#define PARENT_READ     readpipe[0]
#define CHILD_WRITE     readpipe[1]
#define CHILD_READ      writepipe[0]
#define PARENT_WRITE    writepipe[1]

#define DUP2CLOSE(oldfd, newfd) (dup2(oldfd, newfd) == newfd && close(oldfd) == 0)
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))

#define PACKET_N 4
#define BUFF_SIZE (64 * 1024) 
/* #define DEBUG 1 */

union packet_size_buff_t {
  char     char_b[PACKET_N];
  uint32_t int_b;
};

struct packet_t {
  char *    data;
  uint32_t  len;
};


void wait_child(void);
struct packet_t * transfer_data(int read_fd, int write_fd, struct packet_t * input_packet);
void read_all_to_packet(int fd, struct packet_t * output_packet);
int read_buff_to_packet(int fd, struct packet_t * packet);
uint32_t write_buff_from_packet(int fd, struct packet_t * packet, uint32_t offset);
struct packet_t * read_packet(int fd);
void write_packet(int fd, struct packet_t * packet);
struct packet_t * create_packet(uint32_t len);
void destroy_packet(struct packet_t * packet);
void resize_packet(struct packet_t * packet, uint32_t new_len);
void read_n_bytes(int fd, char * buff, uint32_t n);
void write_n_bytes(int fd, char * buff, uint32_t n);
void set_nonblocking(int fd, int val);
#ifdef DEBUG
void dump(char * value, size_t size);
#endif


int main(int argc, char *argv[]) {
  int readpipe[2], writepipe[2];
  pid_t cpid;

#ifdef DEBUG
  fprintf(stderr, "init\n");
#endif

  if(!(1 < argc && argc < 64)) {
    fprintf(stderr, "incorrect arguments\n");
    fprintf(stderr, "Usage: estdinout <program_name> [program_args]\n");
    _exit(EXIT_FAILURE);
  }

  if (pipe(readpipe) == -1 || pipe(writepipe) == -1) {
    perror("pipe");
    _exit(EXIT_FAILURE);
  }

  cpid = fork();
  if (cpid == -1) { perror("fork"); exit(EXIT_FAILURE); }

  if (cpid == 0) {
    char *cmd = argv[1];
    char *exec_args[64] = {0};
    int i;
  
    for (i = 0; i < argc - 1; i++) {
      exec_args[i] = argv[i+1];
    }
    
    close(PARENT_READ);
    close(PARENT_WRITE);

    /* CHILD_READ  = STDIN  to the exec'd process.
       CHILD_WRITE = STDOUT to the exec'd process. */
    if (!DUP2CLOSE(CHILD_READ,   STDIN_FILENO) ||
        !DUP2CLOSE(CHILD_WRITE, STDOUT_FILENO)) {
      perror("dup2 or close");
      _exit(EXIT_FAILURE);
    }

#ifdef DEBUG
    fprintf(stderr, "run %s\n", cmd);
#endif

    if (execvp(cmd, exec_args) == -1) {
      perror("execvp");
    }
    _exit(EXIT_FAILURE);
  }
  else {
    /* Original Parent Process */
    struct packet_t * input_packet;
    struct packet_t * output_packet;

#ifdef DEBUG
    fprintf(stderr, "hello\n");
#endif

    close(CHILD_READ);
    close(CHILD_WRITE);

    input_packet  = read_packet(STDIN_FILENO);
    output_packet = transfer_data(PARENT_READ, PARENT_WRITE, input_packet);

    write_packet(STDOUT_FILENO, output_packet);

    destroy_packet(input_packet);
    destroy_packet(output_packet);

    wait_child();

    exit(EXIT_SUCCESS);
  }
}


void wait_child(void) {
  int status;
  wait(&status);
  if(status) {
    fprintf(stderr, "child error status %i\n", status);
    _exit(EXIT_FAILURE);
  }
}


struct packet_t * transfer_data(int read_fd, int write_fd, struct packet_t * input_packet) {
  uint32_t written_data_len = 0;
  struct packet_t * output_packet = create_packet(0);

  set_nonblocking(read_fd, 1);
  set_nonblocking(write_fd, 1);

  while(written_data_len < input_packet->len) {
    read_all_to_packet(read_fd, output_packet);
    written_data_len += write_buff_from_packet(write_fd, input_packet, written_data_len);
  }
  close(write_fd);

  set_nonblocking(read_fd, 0);

  read_all_to_packet(read_fd, output_packet);
  close(read_fd);

  return output_packet;
}


/* packets io */
void read_all_to_packet(int fd, struct packet_t * packet) {
    while(read_buff_to_packet(fd, packet)) {
#ifdef DEBUG
      fprintf(stderr, "packet\n");
      dump(packet->data, packet->len);
#endif
    }
}


int read_buff_to_packet(int fd, struct packet_t * packet) {
  char     buff[BUFF_SIZE];
  uint32_t old_packet_len = packet->len;
  ssize_t  bytes_read;

  bytes_read = read(fd, buff, BUFF_SIZE);
#ifdef DEBUG
  fprintf(stderr, "read %ld bytes\n", bytes_read);
#endif
  if(bytes_read == -1) {
    if(errno == EAGAIN) {
      return 0;
    }
    perror("read data");
    _exit(EXIT_FAILURE);
  }

  if(bytes_read > 0) {
#ifdef DEBUG
    fprintf(stderr, "buff\n");
    dump(buff, bytes_read);
#endif
    resize_packet(packet, old_packet_len + bytes_read);
    memcpy(packet->data + old_packet_len, buff, bytes_read);
  }

  return bytes_read;
}


uint32_t write_buff_from_packet(int fd, struct packet_t * packet, uint32_t offset) {
  uint32_t remain_bytes_is_packet = packet->len - offset;
  uint32_t bytes_to_write         = MIN(remain_bytes_is_packet, BUFF_SIZE);
  ssize_t  written_bytes          = write(fd, packet->data + offset, bytes_to_write);

#ifdef DEBUG
  fprintf(stderr, "written %ld bytes\n", written_bytes);
#endif
  if(written_bytes == -1) {
    if(errno == EAGAIN) {
      return 0;
    }
    perror("write data");
    _exit(EXIT_FAILURE);
  }

  return written_bytes;
}


struct packet_t * read_packet(int fd) {
    union packet_size_buff_t size_buff;
    struct packet_t * packet;

    if(read(fd, size_buff.char_b, PACKET_N) < PACKET_N) {
      perror("read input data size");
      _exit(EXIT_FAILURE);
    }

    packet = create_packet(ntohl(size_buff.int_b));

#ifdef DEBUG
    fprintf(stderr, "read packet with len %u\n", packet->len);
#endif

    read_n_bytes(fd, packet->data, packet->len);
    return packet;
}


void write_packet(int fd, struct packet_t * packet) {
    union packet_size_buff_t size_buff;

#ifdef DEBUG
    fprintf(stderr, "writing packet with len %u\n", packet->len);
#endif

    size_buff.int_b = htonl(packet->len);
    if(write(fd, size_buff.char_b, PACKET_N) < PACKET_N) {
      perror("write output data size");
      _exit(EXIT_FAILURE);
    }

    write_n_bytes(fd, packet->data, packet->len);
}

/* packets managment */
struct packet_t * create_packet(uint32_t len) {
  struct packet_t * packet = malloc(sizeof(struct packet_t));
  packet->data = malloc(len);
  packet->len = len;
  return packet;
}


void destroy_packet(struct packet_t * packet) {
  free(packet->data);
  free(packet);
}


void resize_packet(struct packet_t * packet, uint32_t new_len) {
  packet->data = realloc(packet->data, new_len);
  packet->len = new_len;
}


/* io */
void read_n_bytes(int fd, char * buff, uint32_t n) {
  ssize_t t;
  if(n == 0) {
    return;
  }
  do {
#ifdef DEBUG
    fprintf(stderr, "reading %u bytes\n", n);
#endif
    t = read(fd, buff, n);
#ifdef DEBUG
    fprintf(stderr, "read %li bytes\n", t);
#endif
    if(t < 1) {
      perror("read input data");
      _exit(EXIT_FAILURE);
    } 
    n -= t;
    buff += t;
  } while(n);
}


void write_n_bytes(int fd, char * buff, uint32_t n) {
  ssize_t t;
  if(n == 0) {
    return;
  }
  do {
#ifdef DEBUG
    fprintf(stderr, "writing %u bytes\n", n);
#endif
    t = write(fd, buff, n);
#ifdef DEBUG
    fprintf(stderr, "written %li bytes\n", t);
#endif
    if(t < 1) {
      perror("write input data");
      _exit(EXIT_FAILURE);
    } 
    n -= t;
    buff += t;
  } while(n);
}

void set_nonblocking(int fd, int val) {
  if(ioctl(fd, FIONBIO, &val) == -1) {
    perror("set_nonblocking");
    _exit(EXIT_FAILURE);
  }
}

#ifdef DEBUG
void dump(char * value, size_t size) {
  int i;
  for(i = 0; i < size; ++i) 
    fprintf(stderr, "%u, ", (uint8_t)value[i]);
  fprintf(stderr, "\n");
}
#endif
