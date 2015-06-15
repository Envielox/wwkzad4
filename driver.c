#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>
#include <unistd.h>

#include "copilot-c99-codegen/copilot.h"

// ---- message variables
const int8_t NoMessage = 0;
const int8_t Prepare = 1;
const int8_t PromiseWithLast = 2;
const int8_t PromiseWithoutLast = 3;
const int8_t Accept = 4;
const int8_t Acknowledge = 5;

// are this just shared with copilot??
uint8_t recv_from;
uint8_t recv_message_type;
int32_t recv_n;
double recv_temp;
int32_t recv_last_n;

// -- driver globla variables
uint8_t my_id;
int friends_amount;
int friends_numbers[10];
int sockfd;

uint8_t send_to;
uint8_t send_message_type;
int32_t send_n;
double send_temp;
int32_t send_last_n;

bool success;
double success_temp;
double success_hum;

// -- driver helper functions

// gets filename of driver i, and puts it in buff
void getpathtofile(int i, char * buff);
void initialize(void);
void cleanup(void);

// gets message and fills "message variables" above
int recv_udp(void);

// sends message based on "send_* variables" above
void send_udp(void);

// -- copilot variables and functions

// variables shared with copilot
double temp;
double hum;

double rand01;

// request that a message be sent
void send_trigger(uint8_t to, uint8_t type, int32_t n, int32_t last_n, double temp)
{
    send_to = to;
    send_message_type = type;
    send_n = n;
    send_temp = temp;
    send_last_n = last_n;
}

// report that consensus has been reached
void success_trigger(double temp, double hum) {
    success = 1;
    success_temp = temp;
    success_hum = hum;
    printf("Results temp = %f, hum = %f\n", temp, hum);
    exit(0);
}

int main(int argc, char ** argv)
{
    srand(time(NULL));
    setvbuf(stdout, NULL, _IONBF, 0);
    // -- parse command line arguments
    if (argc < 2) {
        fprintf(stderr, "You need to pass more arguments\n");
        exit(1);
    }


    friends_amount = argc - 2;
    my_id = atoi(argv[1]);

    for (int i = 0; i < friends_amount; i++)
    {
        friends_numbers[i] = atoi(argv[2 + i]);
    }

    // --- SOCKET TIME ---
    initialize();

    scanf("%*s %*s %lf %lf\n", &temp, &hum);

    /*if (my_id == 0) // for debug
    {
        send_trigger(1,Prepare,3,5,4.2);
        send_udp();
    }*/

    printf("Starting Id: %d, temp: %f, hum: %f\n", my_id, temp, hum);

    success = false;
    while(!success) {
        int tmp = recv_udp();
        if (tmp > 0)
        {
            // do something with data we have (or not)
            printf("I have got: from: %d, m_t: %d, N: %d, last_N: %d, Temp: %f\n", recv_from, recv_message_type, recv_n, recv_last_n, recv_temp);
        }
        else
        {
            printf("I've got nothing\n"); // for debug
        }
        rand01 = (double)rand() / RAND_MAX;
        step();
        send_udp();
        /*sleep(1);*/
        struct timespec delay = { .tv_sec = 0, .tv_nsec = 500000000};
        nanosleep(&delay, 0);
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
    t.tv_sec = 0;
    t.tv_usec = 0;

    fd_set rfds;
    FD_ZERO(&rfds);
    FD_SET(sockfd, &rfds);

    int select_val = select(sockfd + 1, &rfds, 0, 0, &t);
    if (select_val > 0)
    {
        int32_t buff[6];

        int retv = recvfrom(sockfd, buff, 6 * sizeof(int32_t), 0, 0, 0);
        if (retv < 0)
        {
            perror("Error at receiving");
            /*exit(1);*/
        }
        recv_from = buff[0];
        recv_message_type = buff[1];
        recv_n = buff[2];
        recv_temp = *(double*)(&buff[3]);
        recv_last_n = buff[5];

        return 1;
    } else {
        recv_message_type = NoMessage;
        return 0;
    }
}

void raw_send_udp(void)
{
    struct sockaddr_un friend;
    friend.sun_family = AF_UNIX;
    char friend_path_to_file[9];
    getpathtofile(send_to, friend_path_to_file);
    strcpy(friend.sun_path, friend_path_to_file);

    int buff[5];
    buff[0] = my_id;
    buff[1] = send_message_type;
    buff[2] = send_n;
    *(double*)(&buff[3]) = send_temp;
    buff[5] = send_last_n;

    printf("Sending my_id: %d, mt: %d, N: %d, last_N: %d, temp: %f\n", my_id, send_message_type, send_n, send_last_n, send_temp);

    int retv = sendto(sockfd, buff, 6 * sizeof(int), 0, (struct sockaddr*)&friend, sizeof(sa_family_t) + strlen(friend_path_to_file));
    if (retv < 0) {
        perror("Error at sendto");
        fprintf(stderr, "Tried to send to %d: %s\n", send_to, friend_path_to_file);
        /*exit(1);*/
    }
}

void send_udp(void)
{
    if (send_message_type == NoMessage)
        return;

    // broadcast
    if (send_message_type == Prepare || send_message_type == Accept)
    {
        for (int i = 0; i < friends_amount; i++)
        {
            send_to = friends_numbers[i];
            raw_send_udp();
        }
    }
    else {
        raw_send_udp();
    }

    send_message_type = NoMessage;
}
