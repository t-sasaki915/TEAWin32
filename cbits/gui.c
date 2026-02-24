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

HANDLE EnableVisualStyles(void)
{
    HMODULE hInstance = LoadLibrary("SHLWAPI.DLL");
    if (hInstance == NULL)
    {
        return INVALID_HANDLE_VALUE;
    }

    char szPath[512];
    if (GetModuleFileNameA(hInstance, szPath, 512) == 0)
    {
        return INVALID_HANDLE_VALUE;
    }

    ACTCTX actCtx;
    ZeroMemory(&actCtx, sizeof(actCtx));
    actCtx.cbSize = sizeof(actCtx);
    actCtx.hModule = hInstance;
    actCtx.lpResourceName = MAKEINTRESOURCE(123);
    actCtx.lpSource = szPath;
    actCtx.dwFlags = ACTCTX_FLAG_RESOURCE_NAME_VALID;

    HANDLE hActCtx = CreateActCtx(&actCtx);
    if (hActCtx == INVALID_HANDLE_VALUE)
    {
        return INVALID_HANDLE_VALUE;
    }

    ULONG_PTR cookie;
    if (!ActivateActCtx(hActCtx, &cookie))
    {
        ReleaseActCtx(hActCtx);
        return INVALID_HANDLE_VALUE;
    }

    return hActCtx;
}
