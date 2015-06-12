#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/un.h>
/*#include <netinet/in.h>*/
#include <string.h>
#include <unistd.h>

int my_id;
int friends_ammount;
int friends_numbers[10];

void getpathtofile(int i, char * buff)
{
    memcpy(buff, "socket", 9);
    sprintf(buff + 6, "%d", i);
}

int main(int argc, char ** argv)
{
    if (argc < 2) {
        fprintf(stderr, "You need to pass more arguments");
        exit(1);
    }

    friends_ammount = argc - 2;
    my_id = atoi(argv[1]);

    for (int i = 0; i < friends_ammount; i++)
    {
        friends_numbers[i] = atoi(argv[2 + i]);
    }

    // --- SOCKET TIME ---

    char pathtofile[9];
    getpathtofile(my_id, pathtofile);
    int sockfd = socket( AF_UNIX, SOCK_DGRAM, 0);
    if (sockfd < 0)
    {
        perror("Failed to create socket");
        exit(1);
    }

    if (my_id == 0)
    {
        struct sockaddr_un friend1;
        friend1.sun_family = AF_UNIX;
        char friend_path_to_file[9];
        getpathtofile(1, friend_path_to_file);
        strcpy(friend1.sun_path, friend_path_to_file);

        char buff[30];
        sprintf(buff, "My number is %d", my_id);

        int retv = sendto(sockfd, buff, strlen(buff), 0, (struct sockaddr*)&friend1, sizeof(sa_family_t) + strlen(friend_path_to_file));
        if (retv < 0)
        {
            perror("Error at sendto");
            exit(1);
        }
        /*printf("sent!\n");*/
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

    while(1) {
        // gonna do some selects
        struct timeval t;
        t.tv_sec = 5;
        t.tv_usec = 0;

        fd_set rfds;
        FD_ZERO(&rfds);
        FD_SET(sockfd, &rfds);

        int select_val = select(sockfd + 1, &rfds, 0, 0, &t);
        if (select_val > 0)
        {

            /*printf("bound %d\n", tmp);*/

            char buff[30];

            retv = recvfrom(sockfd, buff, 30, 0,  0 , 0);
            if (recv < 0)
            {
                perror("Error at receiving");
                exit(1);
            }
            printf("what i've got: %s\n", buff);
            break;
        } else {
            printf("I have nothing\n");
        }

    }

    unlink(pathtofile);
    return 0;
}
