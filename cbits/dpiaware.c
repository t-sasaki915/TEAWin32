#include <windows.h>

typedef UINT(WINAPI *PGET_DPI_FOR_WINDOW)(HWND);
typedef BOOL(WINAPI *PSET_PROCESS_DPI_AWARENESS_CONTEXT)(void *);

PGET_DPI_FOR_WINDOW GET_DPI_FOR_WINDOW_FUNC = NULL;

void InitialiseDPIAwareFunctions(void)
{
    HMODULE user32 = GetModuleHandleW(L"user32.dll");
    if (user32 == NULL)
    {
        SetProcessDPIAware();
        return;
    }

    FARPROC getDpiForWindowAddr = GetProcAddress(user32, "GetDpiForWindow");
    if (getDpiForWindowAddr == NULL)
    {
        return;
    }

    GET_DPI_FOR_WINDOW_FUNC = (PGET_DPI_FOR_WINDOW)getDpiForWindowAddr;
}

void EnableDPIAware(void)
{
    HMODULE user32 = GetModuleHandleW(L"user32.dll");
    if (user32 == NULL)
    {
        SetProcessDPIAware();
        return;
    }

    FARPROC setProcessDpiAwarenessContextPtr = GetProcAddress(user32, "SetProcessDpiAwarenessContext");
    if (setProcessDpiAwarenessContextPtr == NULL)
    {
        SetProcessDPIAware();
        return;
    }

    PSET_PROCESS_DPI_AWARENESS_CONTEXT setProcessDpiAwarenessContextFunc =
        (PSET_PROCESS_DPI_AWARENESS_CONTEXT)setProcessDpiAwarenessContextPtr;

    setProcessDpiAwarenessContextFunc((void *)-4); // DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2
}

HICON GetHighDPIIcon(SHSTOCKICONID siid)
{
    DWORD structSize = 544;

    SHSTOCKICONINFO iconInfo;
    iconInfo.cbSize = structSize;

    SHGetStockIconInfo(siid, SHGSI_ICON, &iconInfo);

    return iconInfo.hIcon;
}

double GetScaleFactorForHWND(HWND hwnd)
{
    int dpi;

    if (GET_DPI_FOR_WINDOW_FUNC != NULL)
    {
        dpi = GET_DPI_FOR_WINDOW_FUNC(hwnd);
    }
    else
    {
        HDC hdc = GetDC(hwnd);
        dpi = GetDeviceCaps(hdc, LOGPIXELSX);
        ReleaseDC(hwnd, hdc);
    }

    return ((double)dpi / 96.0);
}

int ScaleValue(double scaleFactor, int v)
{
    return ((int)((v * scaleFactor) + 0.5));
}
