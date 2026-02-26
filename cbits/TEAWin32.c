#include "TEAWin32.h"
#include "Cache.h"

#include <wchar.h>
#include <windows.h>

DWORD TEAWIN32_INSTANCE_PID;
wchar_t TEAWIN32_INSTANCE_PID_STR[9];
HINSTANCE TEAWIN32_MAIN_INSTANCE;
WNDPROC TEAWIN32_WNDPROC;

void InitialiseTEAWin32C(WNDPROC wndProc)
{
    TEAWIN32_INSTANCE_PID = GetCurrentProcessId();

    swprintf(TEAWIN32_INSTANCE_PID_STR, 9, L"%08X", TEAWIN32_INSTANCE_PID);

    TEAWIN32_MAIN_INSTANCE = GetModuleHandleW(NULL);

    TEAWIN32_WNDPROC = wndProc;
}

LRESULT CALLBACK CallHaskellWndProc(HWND hwnd, UINT wMsg, WPARAM wParam, LPARAM lParam)
{
    if (TEAWIN32_WNDPROC != NULL)
    {
        switch (wMsg)
        {
            case WM_DESTROY:
            case WM_COMMAND:
            case WM_ERASEBKGND:
            case WM_DPICHANGED:
                return TEAWIN32_WNDPROC(hwnd, wMsg, wParam, lParam);
        }
    }

    return DefWindowProcW(hwnd, wMsg, wParam, lParam);
}

void FinaliseTEAWin32C(void)
{
    FinaliseClassCache();
    FinaliseFontCache();
    FinaliseCursorCache();
    FinaliseIconCache();
}
