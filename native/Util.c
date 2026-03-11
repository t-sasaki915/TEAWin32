#include "Error.h"
#include "TEAWin32.h"

#include <stdio.h> // IWYU pragma: keep
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

BOOL EnableVisualStyles(void)
{
    DEBUG_LOG(L"Enabling Visual Styles.");

    HMODULE hInstance = LoadLibraryW(L"SHLWAPI.DLL");
    if (hInstance == NULL)
    {
        WIN32_ERROR(L"Failed to load SHLWAPI.DLL");
        return FALSE;
    }

    wchar_t szPath[512];
    if (GetModuleFileNameW(hInstance, szPath, ARRAYSIZE(szPath)) == 0)
    {
        WIN32_ERROR(L"GetModuleFileNameW Failed");
        return FALSE;
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
        WIN32_ERROR(L"CreateActCtxW Failed");
        return FALSE;
    }

    ULONG_PTR cookie;
    if (!ActivateActCtx(hActCtx, &cookie))
    {
        WIN32_ERROR(L"ActivateActCtx Failed");
        ReleaseActCtx(hActCtx);
        return FALSE;
    }

    DEBUG_LOG(L"Enabled Visual Styles.");

    return TRUE;
}
