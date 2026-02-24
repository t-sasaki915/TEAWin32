#include <windows.h>

double GetScaleFactorForHWND(HWND hwnd)
{
    HDC hdc = GetDC(hwnd);
    int dpiY = GetDeviceCaps(hdc, 90);
    ReleaseDC(hwnd, hdc);

    return ((double) dpiY / 96.0);
}
