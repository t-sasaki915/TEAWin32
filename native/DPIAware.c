#include "DPIAware.h"
#include "Event.h"
#include "Registry.h"
#include "TEAWin32.h"

#include <stdio.h>
#include <windows.h>

typedef UINT(WINAPI *PGET_DPI_FOR_WINDOW)(HWND);
typedef BOOL(WINAPI *PSET_PROCESS_DPI_AWARENESS_CONTEXT)(void *);

static PGET_DPI_FOR_WINDOW GET_DPI_FOR_WINDOW_FUNC = NULL;

void InitialiseDPIAwareFunctions(void)
{
    HMODULE user32 = GetModuleHandleW(L"user32.dll");
    if (user32 == NULL)
    {
        NotifyFatalError(L"Failed to load user32.dll", L"InitialiseDPIAwareFunctions (DPIAware.c)");
        return;
    }

    FARPROC setProcessDpiAwarenessContextPtr = GetProcAddress(user32, "SetProcessDpiAwarenessContext");
    if (setProcessDpiAwarenessContextPtr != NULL)
    {
        PSET_PROCESS_DPI_AWARENESS_CONTEXT setProcessDpiAwarenessContextFunc =
            (PSET_PROCESS_DPI_AWARENESS_CONTEXT)setProcessDpiAwarenessContextPtr;

        setProcessDpiAwarenessContextFunc((void *)-4); // DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2

        DEBUG_LOG(L"SetProcessDpiAwarenessContext found. Using DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2.");
    }
    else
    {
        SetProcessDPIAware();

        DEBUG_LOG(L"SetProcessDpiAwarenessContext not found. Using SetProcessDPIAware().");
    }

    FARPROC getDpiForWindowAddr = GetProcAddress(user32, "GetDpiForWindow");
    if (getDpiForWindowAddr != NULL)
    {
        GET_DPI_FOR_WINDOW_FUNC = (PGET_DPI_FOR_WINDOW)getDpiForWindowAddr;

        DEBUG_LOG(L"GetDpiForWindow found.");
    }
}

HICON GetHighDPIIcon(SHSTOCKICONID siid)
{
    SHSTOCKICONINFO iconInfo;
    iconInfo.cbSize = sizeof(iconInfo);

    SHGetStockIconInfo(siid, SHGSI_ICON, &iconInfo);

    return iconInfo.hIcon;
}

int GetDPI(HWND hwnd)
{
    if (GET_DPI_FOR_WINDOW_FUNC == NULL)
    {
        DEBUG_LOG(L"GET_DPI_FOR_WINDOW_FUNC == NULL. Checking DPI for HWND %p using GetDeviceCaps.", (void *)hwnd);

        HDC hdc = GetDC(hwnd);
        int dpi = GetDeviceCaps(hdc, LOGPIXELSX);
        ReleaseDC(hwnd, hdc);
        return dpi;
    }

    DEBUG_LOG(L"GET_DPI_FOR_WINDOW_FUNC /= NULL. Checking DPI for HWND %p using GetDpiForWindow.", (void *)hwnd);

    return GET_DPI_FOR_WINDOW_FUNC(hwnd);
}

static inline int GetCachedDpi(HWND hwnd)
{
    HWNDRegistryEntry *entry = GetHWNDRegistryEntry(hwnd);

    if (entry == NULL)
    {
        NotifyFatalError(L"HWNDRegistryEntry was NULL", L"GetCachedDpi (DPIAware.c)");
        return 96;
    }

    DEBUG_LOG(L"Checking DPI for HWND %p using HWNDRegistry.", (void *)hwnd);

    return entry->dpi;
}

int ResolvePixelForDpi(ScalableValue scalable, int dpi)
{
    if (!scalable.isScalable)
    {
        return ((int)scalable.value);
    }

    return MulDiv((int)scalable.value, dpi, 96);
}

int ResolvePixelForHWND(ScalableValue scalable, HWND hwnd)
{
    return ResolvePixelForDpi(scalable, GetCachedDpi(hwnd));
}

int ResolvePointForDpi(ScalableValue scalable, int dpi)
{
    if (!scalable.isScalable)
    {
        return ((int)scalable.value);
    }

    return -MulDiv((int)scalable.value, dpi, 72);
}

int ResolvePointForHWND(ScalableValue scalable, HWND hwnd)
{
    return ResolvePointForDpi(scalable, GetCachedDpi(hwnd));
}
