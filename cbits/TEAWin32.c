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
int TEAWIN32_ACTIVE_WINDOW_COUNT = 0;

static HaskellWndProcCallbacks HASKELL_WNDPROC_CALLBACKS;
static HWNDIDMapEntry HWND_ID_MAP[TEAWIN32_HWND_ID_MAP_MAX];

void InitialiseTEAWin32C(HaskellWndProcCallbacks *haskellWndProcCallbacks)
{
    TEAWIN32_INSTANCE_PID = GetCurrentProcessId();

    swprintf(TEAWIN32_INSTANCE_PID_STR, 9, L"%08X", TEAWIN32_INSTANCE_PID);

    TEAWIN32_MAIN_INSTANCE = GetModuleHandleW(NULL);

    HASKELL_WNDPROC_CALLBACKS = *haskellWndProcCallbacks;
}

HWND GetHWNDFromUniqueId(int uniqueId) // TODO: SLOW
{
    for (int i = 0; i < TEAWIN32_HWND_ID_MAP_MAX; i++)
    {
        if (HWND_ID_MAP[i].uniqueId == uniqueId)
        {
            return HWND_ID_MAP[i].correspondingHWND;
        }
    }

    return NULL;
}

int GetUniqueIdFromHWND(HWND hwnd)
{
    for (int i = 0; i < TEAWIN32_HWND_ID_MAP_MAX; i++)
    {
        if (HWND_ID_MAP[i].correspondingHWND == hwnd)
        {
            return HWND_ID_MAP[i].uniqueId;
        }
    }

    return 0;
}

void RegisterHWNDUniqueId(HWND hwnd, int uniqueId)
{
    for (int i = 0; i < TEAWIN32_HWND_ID_MAP_MAX; i++)
    {
        if (HWND_ID_MAP[i].correspondingHWND == NULL)
        {
            HWND_ID_MAP[i].uniqueId = uniqueId;
            HWND_ID_MAP[i].correspondingHWND = hwnd;

            return;
        }
    }
}

void UnregisterHWNDUniqueId(HWND hwnd)
{
    for (int i = 0; i < TEAWIN32_HWND_ID_MAP_MAX; i++)
    {
        if (HWND_ID_MAP[i].correspondingHWND == hwnd)
        {
            HWND_ID_MAP[i].uniqueId = 0;
            HWND_ID_MAP[i].correspondingHWND = NULL;
        }
    }
}

LRESULT HandleCallbackResult(HWND hwnd, UINT wMsg, WPARAM wParam, LPARAM lParam, int result)
{
    switch (result)
    {
        case 0: {
            return 0;
        }
        case 1: {
            return DefWindowProcW(hwnd, wMsg, wParam, lParam);
        }
        default: {
            return DefWindowProcW(hwnd, wMsg, wParam, lParam);
        }
    }
}

LRESULT CALLBACK TEAWin32WndProc(HWND hwnd, UINT wMsg, WPARAM wParam, LPARAM lParam)
{
    int uniqueId = GetUniqueIdFromHWND(hwnd);
    int callbackResult;

    switch (wMsg)
    {
        case WM_DESTROY: {
            callbackResult = HASKELL_WNDPROC_CALLBACKS.onWindowDestroy(uniqueId);

            TEAWIN32_ACTIVE_WINDOW_COUNT--;

            if (TEAWIN32_ACTIVE_WINDOW_COUNT == 0)
            {
                PostQuitMessage(0);
            }

            break;
        }
        default: {
            return DefWindowProcW(hwnd, wMsg, wParam, lParam);
        }
    }

    return HandleCallbackResult(hwnd, wMsg, wParam, lParam, callbackResult);
}

void FinaliseTEAWin32C(void)
{
    FinaliseClassCache();
    FinaliseFontCache();
    FinaliseCursorCache();
    FinaliseIconCache();
}
