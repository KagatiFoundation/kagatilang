#include "/Users/rigelstar/Desktop/Projects/raylib/src/raylib.h"

void __RAYLIB_init_window(int w, int h, char* title) {
    InitWindow(w, h, title);
}

void __RAYLIB_close_window() {
    CloseWindow();
}

int __RAYLIB_window_should_close() {
    if (WindowShouldClose()) {
        return 1;
    }
    else {
        return 0;
    }
}

void __RAYLIB_begin_drawing() {
    BeginDrawing();
}

void __RAYLIB_end_drawing() {
    EndDrawing();
}

void __RAYLIB_clear_bg() {
    ClearBackground(WHITE);
}