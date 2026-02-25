#include "TEAWin32.h"

#include <windows.h>

BOOL CALLBACK MakeWindowTaskModalCallback(HWND hwnd, LPARAM lParam)
{
    DWORD pid;
    GetWindowThreadProcessId(hwnd, &pid);
    if (pid == TEAWIN32_INSTANCE_PID && hwnd != (HWND)lParam)
    {
        EnableWindow(hwnd, FALSE);
    }

    return TRUE;
}

void MakeWindowTaskModal(HWND hwnd)
{
    EnumWindows(MakeWindowTaskModalCallback, (LPARAM)hwnd);
}
