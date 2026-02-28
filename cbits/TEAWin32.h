#ifndef TEAWIN32_H
#define TEAWIN32_H

#include <windows.h>

#define TEAWIN32_WINDOW_CLASS_IDENTIFIER L"_TEAWIN32_"
#define TEAWIN32_WINDOW_CLASS_IDENTIFIER_LENGTH 10

extern DWORD TEAWIN32_INSTANCE_PID;
extern wchar_t TEAWIN32_INSTANCE_PID_STR[9];
extern HINSTANCE TEAWIN32_MAIN_INSTANCE;
extern WNDPROC TEAWIN32_WNDPROC;

LRESULT CALLBACK CallHaskellWndProc(HWND hwnd, UINT wMsg, WPARAM wParam, LPARAM lParam);

void RegisterHWNDUniqueId(HWND hwnd, int uniqueId);

void UnregisterHWNDUniqueId(HWND hwnd);

HWND GetHWNDFromUniqueId(int uniqueId);

#endif
