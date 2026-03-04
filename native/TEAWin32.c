#include "TEAWin32.h"
#include "Cache.h"
#include "DPIAware.h"
#include "Event.h"
#include "Registry.h"
#include "Util.h"

#include <commctrl.h>
#include <wchar.h>
#include <windows.h>

DWORD TEAWIN32_INSTANCE_PID;
wchar_t TEAWIN32_INSTANCE_PID_STR[9];
HINSTANCE TEAWIN32_MAIN_INSTANCE;
int TEAWIN32_ACTIVE_WINDOW_COUNT = 0;

void InitialiseTEAWin32C(TEAWin32Settings *settings, PEVENTENQUEUER eventEnqueuerPtr)
{
    InitialiseEvent(eventEnqueuerPtr);

    InitialiseDPIAwareFunctions();

    if (settings->useVisualStyles)
    {
        EnableVisualStyles();
    }

    TEAWIN32_INSTANCE_PID = GetCurrentProcessId();

    swprintf(TEAWIN32_INSTANCE_PID_STR, 9, L"%08X", TEAWIN32_INSTANCE_PID);

    TEAWIN32_MAIN_INSTANCE = GetModuleHandleW(NULL);

    EventQueueEntry testEntry;
    ZeroMemory(&testEntry, sizeof(testEntry));
    testEntry.eventType = EVENT_TYPE_TEST_EVENT;
    QueueEvent(&testEntry);
}

LRESULT CALLBACK TEAWin32WndProc(HWND hwnd, UINT wMsg, WPARAM wParam, LPARAM lParam)
{
    // HWNDRegistryEntry *entry = GetHWNDRegistryEntry(hwnd);
    // int uniqueId = entry->uniqueId;

    switch (wMsg)
    {
        case WM_NCDESTROY: {
            UnregisterHWNDFromRegistry(hwnd);

            TEAWIN32_ACTIVE_WINDOW_COUNT--;

            if (TEAWIN32_ACTIVE_WINDOW_COUNT == 0)
            {
                PostQuitMessage(0);
            }
        }
        default: {
            return DefWindowProcW(hwnd, wMsg, wParam, lParam);
        }
    }
}

LRESULT CALLBACK
SubclassWndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam, UINT_PTR uIdSubclass, DWORD_PTR dwRefData)
{
    (void)dwRefData;

    if (uMsg == WM_NCDESTROY)
    {
        UnregisterHWNDFromRegistry(hwnd);

        RemoveWindowSubclass(hwnd, SubclassWndProc, uIdSubclass);
    }

    return DefSubclassProc(hwnd, uMsg, wParam, lParam);
}

void FinaliseTEAWin32C(void)
{
    FinaliseClassCache();
    FinaliseFontCache();
    FinaliseCursorCache();
    FinaliseIconCache();
}
