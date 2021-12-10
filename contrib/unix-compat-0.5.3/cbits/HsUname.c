/*
 * For details of what's going on here, see the following URL:
 *
 * http://msdn.microsoft.com/en-us/library/ms724429(v=vs.85).aspx
 */

#include <windows.h>
#include <tchar.h>
#include <stdio.h>

#ifdef _MSC_VER
# include <strsafe.h>
#else

static void StringCchCopy(char *dest, size_t bufsize, const char *src)
{
    strcpy(dest, src);
}

static void StringCchCat(char *dest, size_t bufsize, const char *src)
{
    strcat(dest, src);
}

#define StringCchPrintf _snprintf

#endif

typedef void (WINAPI *PGNSI)(LPSYSTEM_INFO);
typedef BOOL (WINAPI *PGPI)(DWORD, DWORD, DWORD, DWORD, PDWORD);

#ifndef PRODUCT_ULTIMATE
# define PRODUCT_ULTIMATE 0x00000001
#endif

#ifndef PRODUCT_PROFESSIONAL
# define PRODUCT_PROFESSIONAL 0x00000030
#endif

#ifndef PRODUCT_HOME_PREMIUM
# define PRODUCT_HOME_PREMIUM 0x00000003
#endif

#ifndef PRODUCT_HOME_BASIC
# define PRODUCT_HOME_BASIC 0x00000002
#endif

#ifndef PRODUCT_BUSINESS
# define PRODUCT_BUSINESS 0x00000006
#endif

#ifndef PRODUCT_ENTERPRISE
# define PRODUCT_ENTERPRISE 0x00000004
#endif

#ifndef PRODUCT_STARTER
# define PRODUCT_STARTER 0x0000000B
#endif

#ifndef PRODUCT_CLUSTER_SERVER
# define PRODUCT_CLUSTER_SERVER 0x00000012
#endif

#ifndef PRODUCT_DATACENTER_SERVER
# define PRODUCT_DATACENTER_SERVER 0x00000008
#endif

#ifndef PRODUCT_DATACENTER_SERVER_CORE
# define PRODUCT_DATACENTER_SERVER_CORE 0x0000000C
#endif

#ifndef PRODUCT_ENTERPRISE_SERVER
# define PRODUCT_ENTERPRISE_SERVER 0x0000000A
#endif

#ifndef PRODUCT_ENTERPRISE_SERVER_CORE
# define PRODUCT_ENTERPRISE_SERVER_CORE 0x0000000E
#endif

#ifndef PRODUCT_ENTERPRISE_SERVER_IA64
# define PRODUCT_ENTERPRISE_SERVER_IA64 0x0000000F
#endif

#ifndef PRODUCT_SMALLBUSINESS_SERVER
# define PRODUCT_SMALLBUSINESS_SERVER 0x00000009
#endif

#ifndef PRODUCT_SMALLBUSINESS_SERVER_PREMIUM
# define PRODUCT_SMALLBUSINESS_SERVER_PREMIUM 0x00000019
#endif

#ifndef PRODUCT_STANDARD_SERVER
# define PRODUCT_STANDARD_SERVER 0x00000007
#endif

#ifndef PRODUCT_STANDARD_SERVER_CORE
# define PRODUCT_STANDARD_SERVER_CORE 0x0000000D
#endif

#ifndef PRODUCT_WEB_SERVER
# define PRODUCT_WEB_SERVER 0x00000011
#endif

#ifndef VER_SUITE_WH_SERVER
# define VER_SUITE_WH_SERVER 0x00008000
#endif

int unixcompat_os_display_string(char *pszOS, size_t BUFSIZE)
{
    OSVERSIONINFOEX osvi;
    SYSTEM_INFO si;
    PGNSI pGNSI;
    PGPI pGPI;
    BOOL bOsVersionInfoEx;
    DWORD dwType;

    ZeroMemory(&si, sizeof(SYSTEM_INFO));
    ZeroMemory(&osvi, sizeof(OSVERSIONINFOEX));

    osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
    bOsVersionInfoEx = GetVersionEx((OSVERSIONINFO*) &osvi);

    if (bOsVersionInfoEx == 0)
        return FALSE;

    // Call GetNativeSystemInfo if supported or GetSystemInfo otherwise.

    pGNSI = (PGNSI) GetProcAddress(
        GetModuleHandle(TEXT("kernel32.dll")),
        "GetNativeSystemInfo");
    if (NULL != pGNSI)
        pGNSI(&si);
    else
        GetSystemInfo(&si);

    if (VER_PLATFORM_WIN32_NT == osvi.dwPlatformId && osvi.dwMajorVersion > 4) {
        StringCchCopy(pszOS, BUFSIZE, TEXT("Microsoft "));

        // Test for the specific product.
        if (osvi.dwMajorVersion == 6) {
            if(osvi.dwMinorVersion == 0) {
                if(osvi.wProductType == VER_NT_WORKSTATION)
                    StringCchCat(pszOS, BUFSIZE, TEXT("Windows Vista "));
                else
                    StringCchCat(pszOS, BUFSIZE, TEXT("Windows Server 2008 "));
            }

            if (osvi.dwMinorVersion == 1) {
                if (osvi.wProductType == VER_NT_WORKSTATION)
                    StringCchCat(pszOS, BUFSIZE, TEXT("Windows 7 "));
                else
                    StringCchCat(pszOS, BUFSIZE, TEXT("Windows Server 2008 R2 "));
            }

            pGPI = (PGPI) GetProcAddress(
                GetModuleHandle(TEXT("kernel32.dll")),
                "GetProductInfo");

            pGPI(osvi.dwMajorVersion, osvi.dwMinorVersion, 0, 0, &dwType);

            switch (dwType) {
                case PRODUCT_ULTIMATE:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Ultimate Edition"));
                    break;
                case PRODUCT_PROFESSIONAL:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Professional"));
                    break;
                case PRODUCT_HOME_PREMIUM:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Home Premium Edition"));
                    break;
                case PRODUCT_HOME_BASIC:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Home Basic Edition"));
                    break;
                case PRODUCT_ENTERPRISE:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise Edition"));
                    break;
                case PRODUCT_BUSINESS:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Business Edition"));
                    break;
                case PRODUCT_STARTER:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Starter Edition"));
                    break;
                case PRODUCT_CLUSTER_SERVER:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Cluster Server Edition"));
                    break;
                case PRODUCT_DATACENTER_SERVER:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Datacenter Edition"));
                    break;
                case PRODUCT_DATACENTER_SERVER_CORE:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Datacenter Edition (core installation)"));
                    break;
                case PRODUCT_ENTERPRISE_SERVER:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise Edition"));
                    break;
                case PRODUCT_ENTERPRISE_SERVER_CORE:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise Edition (core installation)"));
                    break;
                case PRODUCT_ENTERPRISE_SERVER_IA64:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise Edition for Itanium-based Systems"));
                    break;
                case PRODUCT_SMALLBUSINESS_SERVER:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Small Business Server"));
                    break;
                case PRODUCT_SMALLBUSINESS_SERVER_PREMIUM:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Small Business Server Premium Edition"));
                    break;
                case PRODUCT_STANDARD_SERVER:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Standard Edition"));
                    break;
                case PRODUCT_STANDARD_SERVER_CORE:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Standard Edition (core installation)"));
                    break;
                case PRODUCT_WEB_SERVER:
                    StringCchCat(pszOS, BUFSIZE, TEXT("Web Server Edition"));
                    break;
            }
        }

        if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2) {
            if (GetSystemMetrics(SM_SERVERR2))
                StringCchCat(pszOS, BUFSIZE, TEXT("Windows Server 2003 R2, "));
            else if (osvi.wSuiteMask & VER_SUITE_STORAGE_SERVER)
                StringCchCat(pszOS, BUFSIZE, TEXT("Windows Storage Server 2003"));
            else if (osvi.wSuiteMask & VER_SUITE_WH_SERVER)
                StringCchCat(pszOS, BUFSIZE, TEXT("Windows Home Server"));
            else if (osvi.wProductType == VER_NT_WORKSTATION &&
                     si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64)
                StringCchCat(pszOS, BUFSIZE, TEXT("Windows XP Professional x64 Edition"));
            else
                StringCchCat(pszOS, BUFSIZE, TEXT("Windows Server 2003, "));

            // Test for the server type.

            if (osvi.wProductType != VER_NT_WORKSTATION) {
                if (si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_IA64) {
                    if(osvi.wSuiteMask & VER_SUITE_DATACENTER)
                        StringCchCat(pszOS, BUFSIZE, TEXT("Datacenter Edition for Itanium-based Systems"));
                    else if(osvi.wSuiteMask & VER_SUITE_ENTERPRISE)
                        StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise Edition for Itanium-based Systems"));
                } else if (si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64) {
                    if(osvi.wSuiteMask & VER_SUITE_DATACENTER)
                        StringCchCat(pszOS, BUFSIZE, TEXT("Datacenter x64 Edition"));
                    else if(osvi.wSuiteMask & VER_SUITE_ENTERPRISE)
                        StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise x64 Edition"));
                    else StringCchCat(pszOS, BUFSIZE, TEXT("Standard x64 Edition"));
                } else {
                    if (osvi.wSuiteMask & VER_SUITE_COMPUTE_SERVER)
                        StringCchCat(pszOS, BUFSIZE, TEXT("Compute Cluster Edition"));
                    else if(osvi.wSuiteMask & VER_SUITE_DATACENTER)
                        StringCchCat(pszOS, BUFSIZE, TEXT("Datacenter Edition"));
                    else if(osvi.wSuiteMask & VER_SUITE_ENTERPRISE)
                        StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise Edition"));
                    else if (osvi.wSuiteMask & VER_SUITE_BLADE)
                        StringCchCat(pszOS, BUFSIZE, TEXT("Web Edition"));
                    else StringCchCat(pszOS, BUFSIZE, TEXT("Standard Edition"));
                }
            }
        }

        if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1) {
            StringCchCat(pszOS, BUFSIZE, TEXT("Windows XP "));

            if (osvi.wSuiteMask & VER_SUITE_PERSONAL)
                StringCchCat(pszOS, BUFSIZE, TEXT("Home Edition"));
            else
                StringCchCat(pszOS, BUFSIZE, TEXT("Professional"));
        }

        if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0) {
            StringCchCat(pszOS, BUFSIZE, TEXT("Windows 2000 "));

            if (osvi.wProductType == VER_NT_WORKSTATION) {
                StringCchCat(pszOS, BUFSIZE, TEXT("Professional"));
            } else {
                if(osvi.wSuiteMask & VER_SUITE_DATACENTER)
                    StringCchCat(pszOS, BUFSIZE, TEXT("Datacenter Server"));
                else if(osvi.wSuiteMask & VER_SUITE_ENTERPRISE)
                    StringCchCat(pszOS, BUFSIZE, TEXT("Advanced Server"));
                else
                    StringCchCat(pszOS, BUFSIZE, TEXT("Server"));
            }
        }

        // Include service pack (if any) and build number.

        if(_tcslen(osvi.szCSDVersion) > 0) {
            StringCchCat(pszOS, BUFSIZE, TEXT(" "));
            StringCchCat(pszOS, BUFSIZE, osvi.szCSDVersion);
        }

        char buf[80];
        StringCchPrintf(buf, 80, TEXT(" (build %d)"), osvi.dwBuildNumber);
        StringCchCat(pszOS, BUFSIZE, buf);

        if (osvi.dwMajorVersion >= 6) {
            if (si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64)
                StringCchCat(pszOS, BUFSIZE, TEXT(", 64-bit"));
            else if (si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_INTEL)
                StringCchCat(pszOS, BUFSIZE, TEXT(", 32-bit"));
        }

        return TRUE;
    } else {
        // This sample does not support this version of Windows.
        return FALSE;
    }
}

int unixcompat_os_version_string(char *ptr, size_t bufsize)
{
    OSVERSIONINFOEX osvi;
    BOOL bOsVersionInfoEx;
    char *szServicePack;

    ZeroMemory(&osvi, sizeof(OSVERSIONINFOEX));
    osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
    bOsVersionInfoEx = GetVersionEx((OSVERSIONINFO*) &osvi);

    if (bOsVersionInfoEx == 0)
        return FALSE;

    if (strncmp(osvi.szCSDVersion, "Service Pack ", 13) == 0)
        szServicePack = "0";
    else
        szServicePack = osvi.szCSDVersion + 13;

    StringCchPrintf(ptr, bufsize, "%ld.%ld.%s.%ld",
                    osvi.dwMajorVersion, osvi.dwMinorVersion, szServicePack,
                    osvi.dwBuildNumber);

    return TRUE;
}

int unixcompat_os_arch_string(char *ptr, size_t bufsize)
{
    SYSTEM_INFO sysInfo;

    GetSystemInfo(&sysInfo);

    switch (sysInfo.wProcessorArchitecture) {
        case PROCESSOR_ARCHITECTURE_INTEL:
            StringCchCopy(ptr, bufsize, "i386");
            break;
        case PROCESSOR_ARCHITECTURE_AMD64:
            StringCchCopy(ptr, bufsize, "x86_64");
            break;
        default:
            StringCchCopy(ptr, bufsize, "unknown");
            break;
    }

    return TRUE;
}

int unixcompat_os_node_name(char *ptr, size_t bufsize)
{
    DWORD sLength;

    sLength = bufsize - 1;
    GetComputerName(ptr, &sLength);

    return TRUE;
}
