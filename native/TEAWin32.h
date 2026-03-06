#ifndef TEAWIN32_H
#define TEAWIN32_H

#include <windows.h>

#define TEAWIN32_WINDOW_CLASS_IDENTIFIER L"_TEAWIN32_"
#define TEAWIN32_WINDOW_CLASS_IDENTIFIER_LENGTH 10

#define TEAWIN32_DEBUG_MODE

#ifdef TEAWIN32_DEBUG_MODE
#define DEBUG_LOG(...)                                                                                                 \
    do                                                                                                                 \
    {                                                                                                                  \
        fwprintf(stdout, L"Debug (C: %hs): ", __func__);                                                               \
        fwprintf(stdout, __VA_ARGS__);                                                                                 \
        fwprintf(stdout, L"\n");                                                                                       \
        fflush(stdout);                                                                                                \
    } while (0)
#else
#define DEBUG_LOG(fmt, ...) ((void)0)
#endif

typedef struct
{
    int (*onWindowDestroy)(int uniqueId);
} HaskellWndProcCallbacks;

typedef struct
{
    BOOL useVisualStyles;
} TEAWin32Settings;

extern DWORD TEAWIN32_INSTANCE_PID;
extern HINSTANCE TEAWIN32_MAIN_INSTANCE;
extern int TEAWIN32_ACTIVE_WINDOW_COUNT;

LRESULT CALLBACK TEAWin32WndProc(HWND hwnd, UINT wMsg, WPARAM wParam, LPARAM lParam);

LRESULT CALLBACK
SubclassWndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam, UINT_PTR uIdSubclass, DWORD_PTR dwRefData);

#endif
