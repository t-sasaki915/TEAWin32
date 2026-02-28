#include "TEAWin32.h"
#include "Cache.h"

#include <wchar.h>
#include <windows.h>

#define TEAWIN32_HWND_ID_MAP_MAX 1024

typedef struct
{
    int uniqueId;
    HWND correspondingHWND;
} HWNDIDMapEntry;

DWORD TEAWIN32_INSTANCE_PID;
wchar_t TEAWIN32_INSTANCE_PID_STR[9];
HINSTANCE TEAWIN32_MAIN_INSTANCE;
WNDPROC TEAWIN32_WNDPROC;
HWNDIDMapEntry TEAWIN32_HWND_ID_MAP[TEAWIN32_HWND_ID_MAP_MAX];

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

void RegisterHWNDUniqueId(HWND hwnd, int uniqueId)
{
    for (int i = 0; i < TEAWIN32_HWND_ID_MAP_MAX; i++)
    {
        if (TEAWIN32_HWND_ID_MAP[i].correspondingHWND == NULL)
        {
            TEAWIN32_HWND_ID_MAP[i].uniqueId = uniqueId;
            TEAWIN32_HWND_ID_MAP[i].correspondingHWND = hwnd;

            return;
        }
    }
}

void UnregisterHWNDUniqueId(HWND hwnd)
{
    for (int i = 0; i < TEAWIN32_HWND_ID_MAP_MAX; i++)
    {
        if (TEAWIN32_HWND_ID_MAP[i].correspondingHWND == hwnd)
        {
            TEAWIN32_HWND_ID_MAP[i].uniqueId = 0;
            TEAWIN32_HWND_ID_MAP[i].correspondingHWND = NULL;
        }
    }
}

HWND GetHWNDFromUniqueId(int uniqueId) // TODO: SLOW
{
    for (int i = 0; i < TEAWIN32_HWND_ID_MAP_MAX; i++)
    {
        if (TEAWIN32_HWND_ID_MAP[i].uniqueId == uniqueId)
        {
            return TEAWIN32_HWND_ID_MAP[i].correspondingHWND;
        }
    }

    return NULL;
}

void FinaliseTEAWin32C(void)
{
    FinaliseClassCache();
    FinaliseFontCache();
    FinaliseCursorCache();
    FinaliseIconCache();
}
