#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <string.h>
#include <unistd.h>

// ---- message variables

int from;
int message_type; // TODO enum
int n;
int v;
int last_n;

// -- driver globla variables
int my_id;
int sockfd;

char send_requested;
int send_to;
int send_message_type; // TODO enum
int send_n;
int send_v;
int send_last_n;

// -- driver helper functions

// gets filename of driver i, and puts it in buff
void getpathtofile(int i, char * buff);
void initialize(void);
void cleanup(void);
int recv_udp(void);
void send_udp(void);

int main(int argc, char ** argv)
{
    // -- parse command line arguments
    if (argc < 2) {
        fprintf(stderr, "You need to pass more arguments");
        exit(1);
    }

    int friends_ammount;
    int friends_numbers[10];

    friends_ammount = argc - 2;
    my_id = atoi(argv[1]);

    for (int i = 0; i < friends_ammount; i++)
    {
        friends_numbers[i] = atoi(argv[2 + i]);
    }

    // --- SOCKET TIME ---
    initialize();

    if (my_id == 0)
    {
        send_requested = 1;
        send_to = 1;
        send_message_type = 2;
        send_n = 3;
        send_v = 4;
        send_last_n = 5;
        send_udp();
    }

    while(1) {
        int tmp = recv_udp();
        if (tmp > 0)
        {
            printf("I have got: %d %d %d %d %d\n", from, message_type, n, v, last_n);
        }
        else
        {
            printf("I've got nothing\n");
        }
    }

    cleanup();
    return 0;
}

void getpathtofile(int i, char * buff)
{
    memcpy(buff, "socket", 6);
    sprintf(buff + 6, "%d", i);
}

void initialize(void)
{
    char pathtofile[9];
    getpathtofile(my_id, pathtofile);

    sockfd = socket(AF_UNIX, SOCK_DGRAM, 0);
    if (sockfd < 0) {
        perror("Failed to create socket");
        exit(1);
    }

    struct sockaddr_un friend1;
    friend1.sun_family = AF_UNIX;
    strcpy(friend1.sun_path, pathtofile);

    int retv = bind(sockfd, (struct sockaddr*)&friend1, sizeof(sa_family_t) + strlen(pathtofile));
    if (retv < 0)
    {
        perror("Error at binding");
        exit(1);
    }
}

void cleanup(void)
{
    char pathtofile[9];
    getpathtofile(my_id, pathtofile);
    unlink(pathtofile);
}

int recv_udp(void)
{
    struct timeval t;
    t.tv_sec = 5;
    t.tv_usec = 0;

    fd_set rfds;
    FD_ZERO(&rfds);
    FD_SET(sockfd, &rfds);

    int select_val = select(sockfd + 1, &rfds, 0, 0, &t);
    if (select_val > 0)
    {
        int buff[6];

        int retv = recvfrom(sockfd, buff, 5 * sizeof(int), 0, 0, 0);
        if (retv < 0)
        {
            perror("Error at receiving");
            exit(1);
        }
        from = buff[0];
        message_type = buff[1];
        n = buff[2];
        v = buff[3];
        last_n = buff[4];

        return 1;
    } else {
        return 0;
    }
}

void send_udp(void)
{
    struct sockaddr_un friend1;
    friend1.sun_family = AF_UNIX;
    char friend_path_to_file[9];
    getpathtofile(send_to, friend_path_to_file);
    strcpy(friend1.sun_path, friend_path_to_file);


    int buff[5];
    buff[0] = my_id;
    buff[1] = send_message_type;
    buff[2] = send_n;
    buff[3] = send_v;
    buff[4] = send_last_n;

    int retv = sendto(sockfd, buff, 5 * sizeof(int), 0, (struct sockaddr*)&friend1, sizeof(sa_family_t) + strlen(friend_path_to_file));
    if (retv < 0) {
        perror("Error at sendto");
        exit(1);
    }
}
