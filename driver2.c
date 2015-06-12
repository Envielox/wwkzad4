#include "copilot.h"

/*
 * in reality 'message' is just a bunch of ints
 *
 * from
 * message_type
 * n
 * v
 * last_n
 *
 * or something alone these lines
 */

// driver only
bool send_requested;
message send_buffer;

bool success;
double success_temp;
double success_hum;

// shared with copilot (should be defined in copilot.h)
/*
bool have_reads;
double temp;
double hum;

bool received;
message received_msg;
*/

// triggers

// request that a message be sent
void send_trigger(message msg) {
    send_requested = 1;
    send_buffer = msg;
}

// report that consensus has been reached
void success_trigger(double temp, double hum) {
    success = 1;
    success_temp = temp;
    success_hum = hum;
}

// helper functions

// if there's a message to be received from udp, receive a single message,
// and parse it into appropriate variables
void receive_udp();

// sends (or broadcasts) a message if there's a message to be sent
void send_udp();

int main(void) {
    // we should probably take fps as an argument, and run this loop at
    // that frequency
    for (;;) {
        // this needs to be non-blocking
        scanf("%*s%*s%lf%lf", &temp, &hum);
        have_reads = 1;
        receive_udp();
        success = 0;
        step();
        send_udp();
        if (success) {
            // we probably want to dump more data
            printf("%lf %lf\n", success_temp, success_hum);
        }
        sleep(42);
    }
    return 0;
}
