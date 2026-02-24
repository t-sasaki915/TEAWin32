#include "DPIAware.h"

#include <windows.h>

#define ERROR_REPORTER_CLASS_NAME L"TEAWin32-ErrorReporter"

#define UI_FONT_SIZE 14
#define EDITOR_FONT_SIZE 12
#define UI_FONT_NAME L"Meiryo UI"
#define EDITOR_FONT_NAME L"Consolas"

#define ERROR_REPORTER_WINDOW_WIDTH 505
#define ERROR_REPORTER_WINDOW_HEIGHT 150
#define ERROR_REPORTER_WINDOW_HEIGHT_WITH_DETAIL_BOX 363

#define CLOSE_BUTTON_LABEL L"Close"
#define CLOSE_BUTTON_X 375
#define CLOSE_BUTTON_Y 70
#define CLOSE_BUTTON_WIDTH 100
#define CLOSE_BUTTON_HEIGHT 28
#define CLOSE_BUTTON_ID 100

#define COPY_BUTTON_LABEL L"Copy Details"
#define COPY_BUTTON_X 185
#define COPY_BUTTON_Y 70
#define COPY_BUTTON_WIDTH 100
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
#define ERROR_ICON_Y 10
#define ERROR_ICON_WIDTH 32
#define ERROR_ICON_HEIGHT 32

#define SHORT_ERROR_MESSAGE_TEXT_X 53
#define SHORT_ERROR_MESSAGE_TEXT_Y 16
#define SHORT_ERROR_MESSAGE_TEXT_COLOUR RGB(0, 0, 0)

HINSTANCE ERROR_REPORTER_INSTANCE;

double SCALE_FACTOR;
HICON ERROR_ICON;
HFONT UI_FONT;
HFONT EDITOR_FONT;

BOOL IS_DETAIL_VISIBLE = FALSE;
HWND DETAIL_BUTTON;
HWND DETAIL_BOX;

LPCWSTR SHORT_ERROR_MESSAGE;
LPCWSTR FULL_ERROR_MSG;

LRESULT CALLBACK WndProc(HWND hwnd, UINT wMsg, WPARAM wParam, LPARAM lParam)
{
    switch (wMsg)
    {
    case WM_PAINT: {
        HDC hdc;
        PAINTSTRUCT ps;

        hdc = BeginPaint(hwnd, &ps);

        DrawIconEx(
            hdc,
            ScaleValue(SCALE_FACTOR, ERROR_ICON_X),
            ScaleValue(SCALE_FACTOR, ERROR_ICON_Y),
            ERROR_ICON,
            ScaleValue(SCALE_FACTOR, ERROR_ICON_WIDTH),
            ScaleValue(SCALE_FACTOR, ERROR_ICON_HEIGHT),
            0,
            NULL,
            3);

        HFONT oldFont = SelectObject(hdc, UI_FONT);

        SetTextColor(hdc, SHORT_ERROR_MESSAGE_TEXT_COLOUR);
        SetBkMode(hdc, 1);

        TextOutW(
            hdc,
            ScaleValue(SCALE_FACTOR, SHORT_ERROR_MESSAGE_TEXT_X),
            ScaleValue(SCALE_FACTOR, SHORT_ERROR_MESSAGE_TEXT_Y),
            SHORT_ERROR_MESSAGE,
            wcslen(SHORT_ERROR_MESSAGE));

        SelectObject(hdc, oldFont);

        EndPaint(hwnd, &ps);

        return 0;
    }

    case WM_COMMAND: {
        switch (LOWORD(wParam))
        {
        case CLOSE_BUTTON_ID: {
            PostQuitMessage(0);

            return 0;
        }

        case COPY_BUTTON_ID: {
            int msgSize = (wcslen(FULL_ERROR_MSG) + 1) * sizeof(LPCSTR);

            HGLOBAL hndl = GlobalAlloc(GMEM_MOVEABLE, msgSize);

            if (hndl != NULL)
            {
                void *mem = GlobalLock(hndl);
                if (mem != NULL)
                {
                    memcpy(mem, FULL_ERROR_MSG, msgSize);
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
            if (IS_DETAIL_VISIBLE)
            {
                SetWindowTextW(DETAIL_BUTTON, DETAIL_BUTTON_LABEL_EXPAND);

                SetWindowPos(
                    hwnd,
                    NULL,
                    0,
                    0,
                    ScaleValue(SCALE_FACTOR, ERROR_REPORTER_WINDOW_WIDTH),
                    ScaleValue(SCALE_FACTOR, ERROR_REPORTER_WINDOW_HEIGHT),
                    SWP_NOMOVE);

                ShowWindow(DETAIL_BOX, SW_HIDE);

                IS_DETAIL_VISIBLE = FALSE;

                return 0;
            }
            else
            {
                SetWindowTextW(DETAIL_BUTTON, DETAIL_BUTTON_LABEL_COLLAPSE);

                SetWindowPos(
                    hwnd,
                    NULL,
                    0,
                    0,
                    ScaleValue(SCALE_FACTOR, ERROR_REPORTER_WINDOW_WIDTH),
                    ScaleValue(SCALE_FACTOR, ERROR_REPORTER_WINDOW_HEIGHT_WITH_DETAIL_BOX),
                    SWP_NOMOVE);

                ShowWindow(DETAIL_BOX, SW_SHOW);

                IS_DETAIL_VISIBLE = TRUE;

                return 0;
            }
        }

        default: {
            return (DefWindowProcW(hwnd, wMsg, wParam, lParam));
        }
        }
    }

    default: {
        return (DefWindowProcW(hwnd, wMsg, wParam, lParam));
    }
    }
}

void ShowErrorReporter(LPCWSTR dialogTitle, LPCWSTR shortMsg, LPCWSTR specificMsgWithStacktrace, LPCWSTR fullMsg)
{
    SHORT_ERROR_MESSAGE = shortMsg;
    FULL_ERROR_MSG = fullMsg;

    ERROR_REPORTER_INSTANCE = GetModuleHandle(NULL);
    ERROR_ICON = GetHighDPIIcon(SIID_ERROR);

    WNDCLASSEXW wndClass;
    ZeroMemory(&wndClass, sizeof(wndClass));
    wndClass.cbSize = sizeof(wndClass);
    wndClass.lpszClassName = ERROR_REPORTER_CLASS_NAME;
    wndClass.style = CS_VREDRAW | CS_HREDRAW;
    wndClass.hInstance = ERROR_REPORTER_INSTANCE;
    wndClass.hIcon = ERROR_ICON;
    wndClass.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
    wndClass.lpfnWndProc = WndProc;

    RegisterClassExW(&wndClass);

    HWND errorReporterWindow = CreateWindowExW(
        WS_EX_TOPMOST | WS_EX_APPWINDOW,
        ERROR_REPORTER_CLASS_NAME,
        dialogTitle,
        WS_CLIPCHILDREN | WS_BORDER | WS_SYSMENU,
        0,
        0,
        0,
        0,
        NULL,
        NULL,
        ERROR_REPORTER_INSTANCE,
        0);

    InitialiseDPIAwareFunctions();
    SCALE_FACTOR = GetScaleFactorForHWND(errorReporterWindow);

    SetWindowPos(
        errorReporterWindow,
        NULL,
        0,
        0,
        ScaleValue(SCALE_FACTOR, ERROR_REPORTER_WINDOW_WIDTH),
        ScaleValue(SCALE_FACTOR, ERROR_REPORTER_WINDOW_HEIGHT),
        SWP_NOMOVE);

    UI_FONT = CreateFontW(
        -ScaleValue(SCALE_FACTOR, UI_FONT_SIZE),
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
    EDITOR_FONT = CreateFontW(
        -ScaleValue(SCALE_FACTOR, EDITOR_FONT_SIZE),
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

    HWND closeButton = CreateWindowW(
        L"BUTTON",
        CLOSE_BUTTON_LABEL,
        WS_TABSTOP | WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON | WS_CLIPSIBLINGS,
        ScaleValue(SCALE_FACTOR, CLOSE_BUTTON_X),
        ScaleValue(SCALE_FACTOR, CLOSE_BUTTON_Y),
        ScaleValue(SCALE_FACTOR, CLOSE_BUTTON_WIDTH),
        ScaleValue(SCALE_FACTOR, CLOSE_BUTTON_HEIGHT),
        errorReporterWindow,
        (HMENU)CLOSE_BUTTON_ID,
        ERROR_REPORTER_INSTANCE,
        NULL);

    SendMessage(closeButton, WM_SETFONT, (WPARAM)UI_FONT, 1);

    HWND copyButton = CreateWindowW(
        L"BUTTON",
        COPY_BUTTON_LABEL,
        WS_TABSTOP | WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON | WS_CLIPSIBLINGS,
        ScaleValue(SCALE_FACTOR, COPY_BUTTON_X),
        ScaleValue(SCALE_FACTOR, COPY_BUTTON_Y),
        ScaleValue(SCALE_FACTOR, COPY_BUTTON_WIDTH),
        ScaleValue(SCALE_FACTOR, COPY_BUTTON_HEIGHT),
        errorReporterWindow,
        (HMENU)COPY_BUTTON_ID,
        ERROR_REPORTER_INSTANCE,
        NULL);

    SendMessage(copyButton, WM_SETFONT, (WPARAM)UI_FONT, 1);

    DETAIL_BUTTON = CreateWindowW(
        L"BUTTON",
        DETAIL_BUTTON_LABEL_EXPAND,
        WS_TABSTOP | WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON | WS_CLIPSIBLINGS,
        ScaleValue(SCALE_FACTOR, DETAIL_BUTTON_X),
        ScaleValue(SCALE_FACTOR, DETAIL_BUTTON_Y),
        ScaleValue(SCALE_FACTOR, DETAIL_BUTTON_WIDTH),
        ScaleValue(SCALE_FACTOR, DETAIL_BUTTON_HEIGHT),
        errorReporterWindow,
        (HMENU)DETAIL_BUTTON_ID,
        ERROR_REPORTER_INSTANCE,
        NULL);

    SendMessage(DETAIL_BUTTON, WM_SETFONT, (WPARAM)UI_FONT, 1);

    DETAIL_BOX = CreateWindowW(
        L"EDIT",
        specificMsgWithStacktrace,
        WS_CHILD | WS_BORDER | WS_VSCROLL | WS_HSCROLL | ES_MULTILINE | ES_READONLY,
        ScaleValue(SCALE_FACTOR, DETAIL_BOX_X),
        ScaleValue(SCALE_FACTOR, DETAIL_BOX_Y),
        ScaleValue(SCALE_FACTOR, DETAIL_BOX_WIDTH),
        ScaleValue(SCALE_FACTOR, DETAIL_BOX_HEIGHT),
        errorReporterWindow,
        NULL,
        ERROR_REPORTER_INSTANCE,
        NULL);

    SendMessage(DETAIL_BOX, WM_SETFONT, (WPARAM)EDITOR_FONT, 1);

    MessageBeep(MB_ICONHAND);

    ShowWindow(errorReporterWindow, SW_SHOWNORMAL);
    UpdateWindow(errorReporterWindow);

    MSG msg;
    BOOL bRet;
    while ((bRet = GetMessage(&msg, NULL, 0, 0)) != 0)
    {
        if (bRet == -1)
        {
            break;
        }

        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    DeleteObject(UI_FONT);
    DeleteObject(EDITOR_FONT);
    DestroyIcon(ERROR_ICON);
}
