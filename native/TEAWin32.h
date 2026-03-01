#ifndef TEAWIN32_H
#define TEAWIN32_H

#include <windows.h>

#define TEAWIN32_WINDOW_CLASS_IDENTIFIER L"_TEAWIN32_"
#define TEAWIN32_WINDOW_CLASS_IDENTIFIER_LENGTH 10

typedef struct
{
    int (*onWindowDestroy)(int uniqueId);
} HaskellWndProcCallbacks;

typedef struct
{
    BOOL useVisualStyles;
} TEAWin32Settings;

extern DWORD TEAWIN32_INSTANCE_PID;
extern wchar_t TEAWIN32_INSTANCE_PID_STR[9];
extern HINSTANCE TEAWIN32_MAIN_INSTANCE;
extern int TEAWIN32_ACTIVE_WINDOW_COUNT;

LRESULT CALLBACK TEAWin32WndProc(HWND hwnd, UINT wMsg, WPARAM wParam, LPARAM lParam);

void RegisterHWNDUniqueId(HWND hwnd, int uniqueId);

void UnregisterHWNDUniqueId(HWND hwnd);

HWND GetHWNDFromUniqueId(int uniqueId);

#endif
