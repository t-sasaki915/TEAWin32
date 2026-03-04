#include "TEAWin32.h"

#include <windows.h>

BOOL CALLBACK MakeWindowTaskModalCallback(HWND hwnd, LPARAM lParam)
{
    DWORD pid;
    GetWindowThreadProcessId(hwnd, &pid);
    if (pid == TEAWIN32_INSTANCE_PID && hwnd != (HWND)lParam)
    {
        EnableWindow(hwnd, FALSE);
    }

    return TRUE;
}

void MakeWindowTaskModal(HWND hwnd)
{
    EnumWindows(MakeWindowTaskModalCallback, (LPARAM)hwnd);
}

void EnableVisualStyles(void)
{
    HMODULE hInstance = LoadLibraryW(L"SHLWAPI.DLL");
    if (hInstance == NULL)
    {
        return;
    }

    wchar_t szPath[512];
    if (GetModuleFileNameW(hInstance, szPath, ARRAYSIZE(szPath)) == 0)
    {
        return;
    }

    ACTCTXW actCtx;
    ZeroMemory(&actCtx, sizeof(actCtx));
    actCtx.cbSize = sizeof(actCtx);
    actCtx.hModule = hInstance;
    actCtx.lpResourceName = MAKEINTRESOURCEW(123);
    actCtx.lpSource = szPath;
    actCtx.dwFlags = ACTCTX_FLAG_RESOURCE_NAME_VALID;

    HANDLE hActCtx = CreateActCtxW(&actCtx);
    if (hActCtx == INVALID_HANDLE_VALUE)
    {
        return;
    }

    ULONG_PTR cookie;
    if (!ActivateActCtx(hActCtx, &cookie))
    {
        ReleaseActCtx(hActCtx);
        return;
    }
}
