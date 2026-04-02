#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>


static void draw_status(void) {
    char status[256];
    snprintf(status, sizeof(status), "[status: %d]", rl_point);

    int width, height;
    rl_get_screen_size(&height, &width);

    int len = strlen(status);
    if (len > width) {
        status[width - 1] = '~';
        status[width] = '\0';
    }

    fprintf(stdout,
        "\033[s"    /* save cursor */
        "\033[1A"   /* up 1 line */
        "\r\033[2K" /* col 0, erase line */
        "%s"        /* status */
        "\033[u",   /* restore cursor */
        status);
    fflush(stdout);
}

static void my_redisplay(void) {
    draw_status();
    rl_redisplay();
}

int main(void) {
    rl_redisplay_function = my_redisplay;

    printf("\n");
    char *line;
    while ((line = readline("prompt > ")) != NULL) {
        printf("\033[2A\r\033[2K"); /* go up 2, clear the status line */
        printf("%s\n", line);
        printf("    = result\n");
        printf("\n");               /* blank line for next status */
        if (*line)
            add_history(line);
        free(line);
    }
    return 0;
}
