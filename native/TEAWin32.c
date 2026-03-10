#include "TEAWin32.h"
#include "Cache.h"
#include "DPIAware.h"
#include "Event.h"
#include "Registry.h"
#include "Util.h"
#include "VirtualDOM.h"

#include <commctrl.h>
#include <stdio.h> // IWYU pragma: keep
#include <windows.h>

DWORD TEAWIN32_INSTANCE_PID;
HINSTANCE TEAWIN32_MAIN_INSTANCE;
HWND TEAWIN32_MANAGEMENT_HWND;
int TEAWIN32_ACTIVE_WINDOW_COUNT = 0;

LRESULT CALLBACK ManagementHWNDWndProc(HWND hwnd, UINT wMsg, WPARAM wParam, LPARAM lParam)
{
    if (wMsg != WM_TEAWIN32_RENDER_REQUEST)
    {
        return DefWindowProcW(hwnd, wMsg, wParam, lParam);
    }

    DEBUG_LOG(L"ManagementHWNDWndProc received WM_TEAWIN32_RENDER_REQUEST.");

    RenderProcedure *procs = (RenderProcedure *)wParam;

    if (procs == NULL)
    {
        NotifyFatalError(
            L"WM_TEAWIN32_RENDER_REQUEST with NULL RenderProcedure.",
            L"ManagementHWNDWndProc (TEAWin32.c)");

        return DefWindowProcW(hwnd, wMsg, wParam, lParam);
    }

    int procedureCount = (int)lParam;

    ExecuteRenderProcedures(procs, procedureCount);

    free(procs);

    return 0;
}

BOOL InitialiseTEAWin32C(TEAWin32Settings *settings, PEVENTENQUEUER eventEnqueuerPtr)
{
    DEBUG_LOG(L"Initialising TEAWin32C.");

    InitialiseEvent(eventEnqueuerPtr);

    if (!InitialiseDPIAwareFunctions())
    {
        return FALSE;
    }

    if (settings->useVisualStyles)
    {
        if (!EnableVisualStyles())
        {
            return FALSE;
        }
    }

    TEAWIN32_INSTANCE_PID = GetCurrentProcessId();

    TEAWIN32_MAIN_INSTANCE = GetModuleHandleW(NULL);

    WNDCLASSEXW wndClass;
    ZeroMemory(&wndClass, sizeof(wndClass));
    wndClass.cbSize = sizeof(wndClass);
    wndClass.lpszClassName = L"TEAWIN32_INTERNAL_MANAGEMENT_HWND";
    wndClass.hInstance = TEAWIN32_MAIN_INSTANCE;
    wndClass.lpfnWndProc = ManagementHWNDWndProc;

    if (!RegisterClassExW(&wndClass))
    {
        NotifyFatalError(
            L"Failed to register TEAWIN32_INTERNAL_MANAGEMENT_HWND class.",
            L"InitialiseTEAWin32C (TEAWin32.c)");
        return FALSE;
    }

    TEAWIN32_MANAGEMENT_HWND =
        CreateWindowW(L"TEAWIN32_INTERNAL_MANAGEMENT_HWND", L"", 0, 0, 0, 0, 0, NULL, NULL, NULL, NULL);

    if (TEAWIN32_MANAGEMENT_HWND == NULL)
    {
        NotifyFatalError(L"Failed to CreateWindowW TEAWIN32_MANAGEMENT_HWND", L"InitialiseTEAWin32C (TEAWin32.c)");
        return FALSE;
    }

    EventQueueEntry testEntry;
    ZeroMemory(&testEntry, sizeof(testEntry));
    testEntry.eventType = EVENT_TYPE_INITIAL_RENDER;
    QueueEvent(&testEntry);

    DEBUG_LOG(L"Initialised TEAWin32C.");

    return TRUE;
}

LRESULT CALLBACK TEAWin32WndProc(HWND hwnd, UINT wMsg, WPARAM wParam, LPARAM lParam)
{
    switch (wMsg)
    {
        case WM_NCDESTROY: {
            if (!UnregisterHWNDFromRegistry(hwnd))
            {
                return DefWindowProcW(hwnd, wMsg, wParam, lParam);
            }

            TEAWIN32_ACTIVE_WINDOW_COUNT--;

            if (TEAWIN32_ACTIVE_WINDOW_COUNT == 0)
            {
                PostQuitMessage(0);
            }

            return 0;
        }
        case WM_ERASEBKGND: {
            HWNDRegistryEntry *regEntry;
            if (!GetHWNDRegistryEntry(hwnd, &regEntry))
            {
                return DefWindowProcW(hwnd, wMsg, wParam, lParam);
            }

            HDC hdc = (HDC)wParam;
            RECT rect;
            GetClientRect(hwnd, &rect);

            HBRUSH backgroundBrush;
            if (regEntry->hasBackgroundColour)
            {
                backgroundBrush = CreateSolidBrush(regEntry->backgroundColour);
            }
            else
            {
                backgroundBrush = GetSysColorBrush(COLOR_WINDOW);
            }

            FillRect(hdc, &rect, backgroundBrush);

            if (regEntry->hasBackgroundColour)
            {
                DeleteObject(backgroundBrush);
            }

            return 1;
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
        if (!UnregisterHWNDFromRegistry(hwnd))
        {
            return DefSubclassProc(hwnd, uMsg, wParam, lParam);
        }

        RemoveWindowSubclass(hwnd, SubclassWndProc, uIdSubclass);
    }

    return DefSubclassProc(hwnd, uMsg, wParam, lParam);
}

void StartWin32MessageLoop(void)
{
    DEBUG_LOG(L"Win32 MessageLoop Started.");

    MSG msg;
    BOOL bRet;
    while ((bRet = GetMessageW(&msg, NULL, 0, 0)) != 0)
    {
        if (bRet == -1)
        {
            break;
        }

        TranslateMessage(&msg);
        DispatchMessageW(&msg);
    }

    DEBUG_LOG(L"Win32 MessageLoop Ended.");
}

void FinaliseTEAWin32C(void)
{
    DEBUG_LOG(L"Finalising TEAWin32C.");

    FinaliseClassCache();
    FinaliseFontCache();
    FinaliseCursorCache();
    FinaliseIconCache();

    DEBUG_LOG(L"Finalised TEAWin32C.");
}
