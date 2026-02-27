#include "Cache.h"
#include "TEAWin32.h"

#include <windows.h>

typedef struct
{
    LPCWSTR managedWindowClassName;
    UINT managedWindowClassStyles;
    DWORD managedWindowExStyles;
    DWORD managedWindowStyles;
    HWND managedWindowParentHWND;
} CreateManagedWindowArgs;

HWND CreateManagedWindow(CreateManagedWindowArgs *args)
{
    wchar_t *className = CreateTEAWin32WindowClassName(args->managedWindowClassName);

    return CreateWindowExW(
        args->managedWindowExStyles,
        className,
        L"",
        args->managedWindowStyles,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        0,
        0,
        args->managedWindowParentHWND,
        NULL,
        TEAWIN32_MAIN_INSTANCE,
        0);
}
