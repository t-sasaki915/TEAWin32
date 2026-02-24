#include <windows.h>

typedef struct
{
    HWND parentHWND;
    HWND *enumBuffer;
    int maxWindowCount;
    int currentCount;
} EnumContext;

BOOL CALLBACK EnumWindowsCallback(HWND hwnd, LPARAM lParam)
{
    EnumContext *enumCtx = (EnumContext *)lParam;

    if (enumCtx->currentCount < enumCtx->maxWindowCount)
    {
        if (GetParent(hwnd) == enumCtx->parentHWND)
        {
            enumCtx->enumBuffer[enumCtx->currentCount++] = hwnd;
        }

        return TRUE;
    }

    return FALSE;
}

int GetImmediateChildWindows(HWND parent, HWND *resultPtr, int maxWindows)
{
    EnumContext enumCtx;
    ZeroMemory(&enumCtx, sizeof(enumCtx));
    enumCtx.parentHWND = parent;
    enumCtx.enumBuffer = resultPtr;
    enumCtx.maxWindowCount = maxWindows;
    enumCtx.currentCount = 0;

    EnumChildWindows(parent, EnumWindowsCallback, (LPARAM)&enumCtx);

    return enumCtx.currentCount;
}

int GetTopLevelWindows(HWND *resultPtr, int maxWindows)
{
    EnumContext enumCtx;
    ZeroMemory(&enumCtx, sizeof(enumCtx));
    enumCtx.parentHWND = NULL;
    enumCtx.enumBuffer = resultPtr;
    enumCtx.maxWindowCount = maxWindows;
    enumCtx.currentCount = 0;

    DWORD threadId = GetCurrentThreadId();
    EnumThreadWindows(threadId, EnumWindowsCallback, (LPARAM)&enumCtx);

    return enumCtx.currentCount;
}

BOOL IsWindowTopLevel(HWND hwnd)
{
    return (GetParent(hwnd) == NULL);
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
