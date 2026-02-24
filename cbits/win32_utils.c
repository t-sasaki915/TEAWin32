#include <windows.h>

typedef UINT(WINAPI *PGET_DPI_FOR_WINDOW)(HWND);

PGET_DPI_FOR_WINDOW GET_DPI_FOR_WINDOW_FUNC = NULL;

void GetGetDpiForWindowFunctionIfExists(void)
{
    HMODULE user32 = GetModuleHandle("user32.dll");
    if (user32 == NULL)
    {
        return;
    }
    FARPROC getDpiForWindowAddr = GetProcAddress(user32, "GetDpiForWindow");
    if (getDpiForWindowAddr == NULL)
    {
        return;
    }

    GET_DPI_FOR_WINDOW_FUNC = (PGET_DPI_FOR_WINDOW)getDpiForWindowAddr;
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
