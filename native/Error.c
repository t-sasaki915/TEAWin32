#include "Error.h"
#include "DPIAware.h"
#include "TEAWin32.h"

#include <stdio.h> // IWYU pragma: keep
#include <windows.h>

#define TEAWIN32_ERROR_LIST_MAX 100

static ErrorListEntry g_teaWin32ErrorList[TEAWIN32_ERROR_LIST_MAX];
static int g_teaWin32ErrorListCount = 0;
static volatile LONG g_teaWin32ErrorListLock = 0;

#define ERROR_REPORTER_CLASS_NAME L"TEAWin32-ErrorReporter"

#define UI_FONT_SIZE 11
#define EDITOR_FONT_SIZE 9
#define UI_FONT_NAME L"Meiryo UI"
#define EDITOR_FONT_NAME L"Consolas"

#define ERROR_REPORTER_WINDOW_TITLE L"TEAWin32 Error"
#define ERROR_REPORTER_WINDOW_WIDTH 505
#define ERROR_REPORTER_WINDOW_HEIGHT 150
#define ERROR_REPORTER_WINDOW_HEIGHT_WITH_DETAIL_BOX 361
#define ERROR_REPORTER_MAIN_TEXT L"An internal error has occurred. Exiting the programme."
#define ERROR_REPORTER_MAIN_TEXT_LENGTH 54

#define CLOSE_BUTTON_LABEL L"Close"
#define CLOSE_BUTTON_X 375
#define CLOSE_BUTTON_Y 70
#define CLOSE_BUTTON_WIDTH 100
#define CLOSE_BUTTON_HEIGHT 28
#define CLOSE_BUTTON_ID 100

#define COPY_BUTTON_LABEL L"Copy Details"
#define COPY_BUTTON_X 180
#define COPY_BUTTON_Y 70
#define COPY_BUTTON_WIDTH 115
#define COPY_BUTTON_HEIGHT 28
#define COPY_BUTTON_ID 101

#define DETAIL_BUTTON_LABEL_EXPAND L"\x25BC Expand Details"
#define DETAIL_BUTTON_LABEL_COLLAPSE L"\x25B2 Collapse Details"
#define DETAIL_BUTTON_X 15
#define DETAIL_BUTTON_Y 70
#define DETAIL_BUTTON_WIDTH 150
#define DETAIL_BUTTON_HEIGHT 28
#define DETAIL_BUTTON_ID 102

#define DETAIL_BOX_X 16
#define DETAIL_BOX_Y 110
#define DETAIL_BOX_WIDTH 458
#define DETAIL_BOX_HEIGHT 200

#define ERROR_ICON_X 10
#define ERROR_ICON_Y 18
#define ERROR_ICON_WIDTH 32
#define ERROR_ICON_HEIGHT 32

#define SHORT_ERROR_MESSAGE_TEXT_X 53
#define SHORT_ERROR_MESSAGE_TEXT_Y 24
#define SHORT_ERROR_MESSAGE_TEXT_COLOUR RGB(0, 0, 0)

#define SCALE(x) ResolvePixel_(x, g_dpi)

static int g_dpi;

static HICON g_errorIcon;
static HFONT g_uiFont;
static HFONT g_editorFont;

static HWND g_errorReporterWindow;
static HWND g_closeButton;
static HWND g_copyButton;
static HWND g_detailButton;
static HWND g_detailBox;

static BOOL g_isDetailVisible = FALSE;

static wchar_t *g_fullErrorLog;

BOOL CheckErrorList(void)
{
    return g_teaWin32ErrorListCount == 0;
}

void ReportError(ErrorListEntry *errorListEntry)
{
    if (g_teaWin32ErrorListCount >= TEAWIN32_ERROR_LIST_MAX)
    {
        DEBUG_LOG(L"TEAWIN32_ERROR_LIST Overflow.");

        return;
    }

    while (InterlockedCompareExchange(&g_teaWin32ErrorListLock, 1, 0) != 0)
    {
        YieldProcessor();
    }

    LPCWSTR permanentErrorDescription = _wcsdup(errorListEntry->errorDescription);
    LPCWSTR permanentErrorLocation = _wcsdup(errorListEntry->errorLocation);

    g_teaWin32ErrorList[g_teaWin32ErrorListCount].errorType = errorListEntry->errorType;
    g_teaWin32ErrorList[g_teaWin32ErrorListCount].errorDescription = permanentErrorDescription;
    g_teaWin32ErrorList[g_teaWin32ErrorListCount].errorLocation = permanentErrorLocation;

    switch (errorListEntry->errorType)
    {
        case ERROR_TYPE_WIN32: {
            g_teaWin32ErrorList[g_teaWin32ErrorListCount].errorExtraInfo.lastWin32ErrorCode =
                errorListEntry->errorExtraInfo.lastWin32ErrorCode;

            break;
        }
        case ERROR_TYPE_FATAL_MEMORY: {
            break;
        }
        case ERROR_TYPE_TEAWIN32: {
            break;
        }
    }

    g_teaWin32ErrorListCount++;

    InterlockedExchange(&g_teaWin32ErrorListLock, 0);

    DEBUG_LOG(
        L"An error has been reported. Description: %ls, Location: %ls.",
        permanentErrorDescription,
        permanentErrorLocation);
}

void FinaliseErrorList(void)
{
    DEBUG_LOG(L"Finalising ERROR_LIST.");

    for (int i = 0; i < g_teaWin32ErrorListCount; i++)
    {
        ErrorListEntry entry = g_teaWin32ErrorList[i];

        DEBUG_LOG(L"Releasing LPCWSTRs of error %ls", entry.errorDescription);

        free((void *)entry.errorDescription);
        free((void *)entry.errorLocation);
    }

    g_teaWin32ErrorListCount = 0;

    DEBUG_LOG(L"Finalised ERROR_LIST.");
}

void CreateErrorLog(wchar_t **pPtr, ErrorListEntry *entry, BOOL isFirstEntry)
{
    wchar_t *p = *pPtr;

    if (!isFirstEntry)
    {
        p += swprintf(p, 2048, L"================================================================\r\n\r\n");
    }

    LPCWSTR errorTypeText;
    switch (entry->errorType)
    {
        case ERROR_TYPE_WIN32: {
            errorTypeText = L"Win32 Error";

            break;
        }
        case ERROR_TYPE_FATAL_MEMORY: {
            errorTypeText = L"Fatal Memory Error";

            break;
        }
        case ERROR_TYPE_TEAWIN32: {
            errorTypeText = L"TEAWin32 Error";

            break;
        }
    }

    p += swprintf(
        p,
        2048,
        L"%ls\r\n    %ls\r\n\r\nOccurred in: %ls\r\n",
        errorTypeText,
        entry->errorDescription,
        entry->errorLocation);

    switch (entry->errorType)
    {
        case ERROR_TYPE_WIN32: {
            p += swprintf(p, 2048, L"GetLastError() = %lu", entry->errorExtraInfo.lastWin32ErrorCode);

            break;
        }
        case ERROR_TYPE_FATAL_MEMORY: {
            break;
        }
        case ERROR_TYPE_TEAWIN32: {
            break;
        }
    }

    p += swprintf(p, 2048, L"\r\n\r\n");

    *pPtr = p;
}

void CreateFullErrorLog(void)
{
    g_fullErrorLog = calloc(g_teaWin32ErrorListCount * 2048, sizeof(wchar_t));

    wchar_t *p = g_fullErrorLog;

    for (int i = 0; i < g_teaWin32ErrorListCount; i++)
    {
        BOOL isFirstEntry = i == 0;
        CreateErrorLog(&p, &g_teaWin32ErrorList[i], isFirstEntry);
    }
}

void StartErrorReporterFallback(void)
{
    wchar_t mainText[g_teaWin32ErrorListCount * 2048];
    swprintf(mainText, g_teaWin32ErrorListCount * 2048, L"%ls\r\n\r\n%ls", ERROR_REPORTER_MAIN_TEXT, g_fullErrorLog);

    MessageBoxW(NULL, mainText, ERROR_REPORTER_WINDOW_TITLE, MB_TASKMODAL | MB_OK | MB_ICONERROR);

    if (g_fullErrorLog != NULL)
    {
        free(g_fullErrorLog);
        g_fullErrorLog = NULL;
    }
}

void DesignErrorReporter(BOOL setMainWindowPos)
{
    g_dpi = GetDPI(g_errorReporterWindow);

    g_uiFont = CreateFontW(
        ResolvePoint_(UI_FONT_SIZE, g_dpi),
        0,
        0,
        0,
        FW_NORMAL,
        FALSE,
        FALSE,
        FALSE,
        DEFAULT_CHARSET,
        OUT_DEFAULT_PRECIS,
        CLIP_DEFAULT_PRECIS,
        CLEARTYPE_QUALITY,
        DEFAULT_PITCH | FF_DONTCARE,
        UI_FONT_NAME);
    g_editorFont = CreateFontW(
        ResolvePoint_(EDITOR_FONT_SIZE, g_dpi),
        0,
        0,
        0,
        FW_NORMAL,
        FALSE,
        FALSE,
        FALSE,
        DEFAULT_CHARSET,
        OUT_DEFAULT_PRECIS,
        CLIP_DEFAULT_PRECIS,
        CLEARTYPE_QUALITY,
        DEFAULT_PITCH | FF_DONTCARE,
        EDITOR_FONT_NAME);

    SendMessageW(g_closeButton, WM_SETFONT, (WPARAM)g_uiFont, 1);
    SendMessageW(g_copyButton, WM_SETFONT, (WPARAM)g_uiFont, 1);
    SendMessageW(g_detailButton, WM_SETFONT, (WPARAM)g_uiFont, 1);
    SendMessageW(g_detailBox, WM_SETFONT, (WPARAM)g_editorFont, 1);

    if (setMainWindowPos)
    {
        SetWindowPos(
            g_errorReporterWindow,
            NULL,
            0,
            0,
            SCALE(ERROR_REPORTER_WINDOW_WIDTH),
            SCALE(ERROR_REPORTER_WINDOW_HEIGHT),
            SWP_NOMOVE);
    }

    SetWindowPos(
        g_closeButton,
        NULL,
        SCALE(CLOSE_BUTTON_X),
        SCALE(CLOSE_BUTTON_Y),
        SCALE(CLOSE_BUTTON_WIDTH),
        SCALE(CLOSE_BUTTON_HEIGHT),
        SWP_NOZORDER);
    SetWindowPos(
        g_copyButton,
        NULL,
        SCALE(COPY_BUTTON_X),
        SCALE(COPY_BUTTON_Y),
        SCALE(COPY_BUTTON_WIDTH),
        SCALE(COPY_BUTTON_HEIGHT),
        SWP_NOZORDER);
    SetWindowPos(
        g_detailButton,
        NULL,
        SCALE(DETAIL_BUTTON_X),
        SCALE(DETAIL_BUTTON_Y),
        SCALE(DETAIL_BUTTON_WIDTH),
        SCALE(DETAIL_BUTTON_HEIGHT),
        SWP_NOZORDER);
    SetWindowPos(
        g_detailBox,
        NULL,
        SCALE(DETAIL_BOX_X),
        SCALE(DETAIL_BOX_Y),
        SCALE(DETAIL_BOX_WIDTH),
        SCALE(DETAIL_BOX_HEIGHT),
        SWP_NOZORDER);
}

LRESULT CALLBACK ErrorReporterWndProc(HWND hwnd, UINT wMsg, WPARAM wParam, LPARAM lParam)
{
    switch (wMsg)
    {
        case WM_DESTROY: {
            PostQuitMessage(0);

            return 0;
        }

        case WM_PAINT: {
            HDC hdc;
            PAINTSTRUCT ps;

            hdc = BeginPaint(hwnd, &ps);

            DrawIconEx(
                hdc,
                SCALE(ERROR_ICON_X),
                SCALE(ERROR_ICON_Y),
                g_errorIcon,
                SCALE(ERROR_ICON_WIDTH),
                SCALE(ERROR_ICON_HEIGHT),
                0,
                NULL,
                DI_NORMAL);

            HFONT oldFont = SelectObject(hdc, g_uiFont);

            SetTextColor(hdc, SHORT_ERROR_MESSAGE_TEXT_COLOUR);
            SetBkMode(hdc, 1);

            TextOutW(
                hdc,
                SCALE(SHORT_ERROR_MESSAGE_TEXT_X),
                SCALE(SHORT_ERROR_MESSAGE_TEXT_Y),
                ERROR_REPORTER_MAIN_TEXT,
                ERROR_REPORTER_MAIN_TEXT_LENGTH);

            SelectObject(hdc, oldFont);

            EndPaint(hwnd, &ps);

            return 0;
        }

        case WM_COMMAND: {
            switch (LOWORD(wParam))
            {
                case CLOSE_BUTTON_ID: {
                    SendMessage(hwnd, WM_CLOSE, 0, 0);

                    return 0;
                }

                case COPY_BUTTON_ID: {
                    int msgSize = (wcslen(g_fullErrorLog) + 1) * sizeof(wchar_t);

                    HGLOBAL hndl = GlobalAlloc(GMEM_MOVEABLE, msgSize);

                    if (hndl != NULL)
                    {
                        void *mem = GlobalLock(hndl);
                        if (mem != NULL)
                        {
                            memcpy(mem, g_fullErrorLog, msgSize);
                            GlobalUnlock(hndl);

                            if (OpenClipboard(hwnd))
                            {
                                EmptyClipboard();
                                HANDLE handle = SetClipboardData(CF_UNICODETEXT, hndl);
                                CloseClipboard();

                                if (handle == NULL)
                                {
                                    GlobalFree(hndl);
                                }
                            }
                            else
                            {
                                GlobalFree(hndl);
                            }
                        }
                    }

                    return 0;
                }

                case DETAIL_BUTTON_ID: {
                    if (g_isDetailVisible)
                    {
                        SetWindowTextW(g_detailButton, DETAIL_BUTTON_LABEL_EXPAND);

                        SetWindowPos(
                            hwnd,
                            NULL,
                            0,
                            0,
                            SCALE(ERROR_REPORTER_WINDOW_WIDTH),
                            SCALE(ERROR_REPORTER_WINDOW_HEIGHT),
                            SWP_NOMOVE);

                        ShowWindow(g_detailBox, SW_HIDE);

                        g_isDetailVisible = FALSE;

                        return 0;
                    }
                    else
                    {
                        SetWindowTextW(g_detailButton, DETAIL_BUTTON_LABEL_COLLAPSE);

                        SetWindowPos(
                            hwnd,
                            NULL,
                            0,
                            0,
                            SCALE(ERROR_REPORTER_WINDOW_WIDTH),
                            SCALE(ERROR_REPORTER_WINDOW_HEIGHT_WITH_DETAIL_BOX),
                            SWP_NOMOVE);

                        ShowWindow(g_detailBox, SW_SHOW);

                        g_isDetailVisible = TRUE;

                        return 0;
                    }
                }

                default: {
                    return (DefWindowProcW(hwnd, wMsg, wParam, lParam));
                }
            }
        }

        case WM_DPICHANGED: {
            RECT *rect = (RECT *)lParam;
            LONG l = rect->left;
            LONG t = rect->top;
            LONG r = rect->right;
            LONG b = rect->bottom;
            SetWindowPos(hwnd, NULL, l, t, r - l, b - t, SWP_NOZORDER | SWP_NOACTIVATE);

            DeleteObject(g_uiFont);
            g_uiFont = NULL;
            DeleteObject(g_editorFont);
            g_editorFont = NULL;
            DestroyIcon(g_errorIcon);
            g_errorIcon = NULL;

            g_errorIcon = GetHighDPIIcon(SIID_ERROR);

            DesignErrorReporter(FALSE);

            InvalidateRect(hwnd, NULL, TRUE);

            return 0;
        }

        default: {
            return (DefWindowProcW(hwnd, wMsg, wParam, lParam));
        }
    }
}

void StartErrorReporter(void)
{
    CreateFullErrorLog();

    if (g_teaWin32MainInstance == NULL)
    {
        g_teaWin32MainInstance = GetModuleHandleW(NULL);
    }

    if (!InitialiseDPIAwareFunctions())
    {
        DEBUG_LOG(L"InitialiseDPIAwareFunctions failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    g_errorIcon = GetHighDPIIcon(SIID_ERROR);
    if (g_errorIcon == NULL)
    {
        DEBUG_LOG(L"GetHighDPIIcon failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    DEBUG_LOG(L"Registering ERROR_REPORTER_CLASS_NAME.");

    WNDCLASSEXW wndClass;
    ZeroMemory(&wndClass, sizeof(wndClass));
    wndClass.cbSize = sizeof(wndClass);
    wndClass.lpszClassName = ERROR_REPORTER_CLASS_NAME;
    wndClass.style = CS_VREDRAW | CS_HREDRAW;
    wndClass.hInstance = g_teaWin32MainInstance;
    wndClass.hIcon = g_errorIcon;
    wndClass.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
    wndClass.lpfnWndProc = ErrorReporterWndProc;

    if (!RegisterClassExW(&wndClass))
    {
        DEBUG_LOG(L"RegisterClassExW failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    DEBUG_LOG(L"Registered ERROR_REPORTER_CLASS_NAME.");

    DEBUG_LOG(L"Creating UI elements.");

    g_errorReporterWindow = CreateWindowExW(
        WS_EX_TOPMOST | WS_EX_APPWINDOW,
        ERROR_REPORTER_CLASS_NAME,
        ERROR_REPORTER_WINDOW_TITLE,
        WS_CLIPCHILDREN | WS_BORDER | WS_SYSMENU,
        0,
        0,
        0,
        0,
        NULL,
        NULL,
        g_teaWin32MainInstance,
        0);

    if (g_errorReporterWindow == NULL)
    {
        DEBUG_LOG(L"ERROR_REPORTER_WINDOW CreateWindowExW failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    g_closeButton = CreateWindowW(
        L"BUTTON",
        CLOSE_BUTTON_LABEL,
        WS_TABSTOP | WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON | WS_CLIPSIBLINGS,
        0,
        0,
        0,
        0,
        g_errorReporterWindow,
        (HMENU)CLOSE_BUTTON_ID,
        g_teaWin32MainInstance,
        NULL);

    if (g_closeButton == NULL)
    {
        DEBUG_LOG(L"CLOSE_BUTTON CreateWindowW failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    g_copyButton = CreateWindowW(
        L"BUTTON",
        COPY_BUTTON_LABEL,
        WS_TABSTOP | WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON | WS_CLIPSIBLINGS,
        0,
        0,
        0,
        0,
        g_errorReporterWindow,
        (HMENU)COPY_BUTTON_ID,
        g_teaWin32MainInstance,
        NULL);

    if (g_copyButton == NULL)
    {
        DEBUG_LOG(L"COPY_BUTTON CreateWindowW failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    g_detailButton = CreateWindowW(
        L"BUTTON",
        DETAIL_BUTTON_LABEL_EXPAND,
        WS_TABSTOP | WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON | WS_CLIPSIBLINGS,
        0,
        0,
        0,
        0,
        g_errorReporterWindow,
        (HMENU)DETAIL_BUTTON_ID,
        g_teaWin32MainInstance,
        NULL);

    if (g_detailButton == NULL)
    {
        DEBUG_LOG(L"DETAIL_BUTTON CreateWindowW failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    g_detailBox = CreateWindowW(
        L"EDIT",
        g_fullErrorLog,
        WS_CHILD | WS_BORDER | WS_VSCROLL | WS_HSCROLL | ES_MULTILINE | ES_READONLY,
        0,
        0,
        0,
        0,
        g_errorReporterWindow,
        NULL,
        g_teaWin32MainInstance,
        NULL);

    if (g_detailBox == NULL)
    {
        DEBUG_LOG(L"DETAIL_BOX CreateWindowW failed. Fallback.");

        StartErrorReporterFallback();
        return;
    }

    DEBUG_LOG(L"Created UI elements.");

    MessageBeep(MB_ICONHAND);

    DesignErrorReporter(TRUE);

    ShowWindow(g_errorReporterWindow, SW_SHOWNORMAL);
    UpdateWindow(g_errorReporterWindow);

    DEBUG_LOG(L"ErrorReporter MessageLoop Started.");

    MSG msg;
    BOOL bRet;
    while ((bRet = GetMessageW(&msg, g_errorReporterWindow, 0, 0)) != 0)
    {
        if (bRet == -1)
        {
            break;
        }

        TranslateMessage(&msg);
        DispatchMessageW(&msg);
    }

    DEBUG_LOG(L"ErrorReporter MessageLoop Ended.");

    if (g_uiFont != NULL)
    {
        DeleteObject(g_uiFont);
        g_uiFont = NULL;
    }
    if (g_editorFont != NULL)
    {
        DeleteObject(g_editorFont);
        g_editorFont = NULL;
    }
    if (g_errorIcon != NULL)
    {
        DestroyIcon(g_errorIcon);
        g_errorIcon = NULL;
    }
    if (g_fullErrorLog != NULL)
    {
        free(g_fullErrorLog);
        g_fullErrorLog = NULL;
    }
}
