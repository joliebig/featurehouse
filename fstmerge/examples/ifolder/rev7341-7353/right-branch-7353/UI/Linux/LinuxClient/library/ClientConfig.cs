

using System;
using System.Collections;
using System.Collections.Specialized;
using System.IO;
using System.Text;
using System.Threading;

namespace Novell.iFolder
{



 public class ClientConfig
 {




  public const string KEY_SHOW_CREATION = "/apps/ifolder3/notification/show_created_dialog";
  public const string KEY_NOTIFY_IFOLDERS = "/apps/ifolder3/notification/new_ifolders";
  public const string KEY_NOTIFY_POLICY_VOILATION = "/apps/ifolder3/notification/policy_violation";
  public const string KEY_NOTIFY_COLLISIONS = "/apps/ifolder3/notification/collisions";
  public const string KEY_NOTIFY_USERS = "/apps/ifolder3/notification/new_users";

  public const string KEY_NOTIFY_MIGRATION_2_X = "/apps/ifolder3/notification/migrate2x";

  public const string KEY_SHOW_SYNC_LOG = "/apps/ifolder3/notification/synclog";


  public const string KEY_SHOW_QUOTA_VIOLATION = "/apps/ifolder3/notifications/quota_violation";
  public const string KEY_SHOW_FILE_SIZE_VOILATION = "/apps/ifolder3/notifications/file_size_violation";
  public const string KEY_SHOW_EXCLUSION_VOILATION = "/apps/ifolder3/notifications/file_exclusion_violation";
  public const string KEY_SHOW_DISK_FULL = "/apps/ifolder3/notifications/disk_full";
  public const string KEY_SHOW_PERMISSION_UNAVAILABLE = "/apps/ifolder3/notifications/permission_unavailable";
  public const string KEY_SHOW_EXCEEDS_PATH_SIZE = "/apps/ifolder3/notifications/exceeds_path_size";





  public const string KEY_SYNC_UNIT = "/apps/ifolder3/synchronization/unit";




  public const string KEY_IFOLDER_WINDOW_X_POS = "/apps/ifolder3/ui/main_window/left";
  public const string KEY_IFOLDER_WINDOW_Y_POS = "/apps/ifolder3/ui/main_window/top";
  public const string KEY_IFOLDER_WINDOW_WIDTH = "/apps/ifolder3/ui/main_window/width";
  public const string KEY_IFOLDER_WINDOW_HEIGHT = "/apps/ifolder3/ui/main_window/height";
  public const string KEY_IFOLDER_WINDOW_VISIBLE = "/apps/ifolder3/ui/main_window/visible";
  public const string KEY_IFOLDER_WINDOW_HIDE = "/apps/ifolder3/ui/main_window/hide";
  public const string KEY_SHOW_SERVER_IFOLDERS = "/apps/ifolder3/ui/main_window/show_available_ifolders";




  public const string KEY_IFOLDER_ACCOUNT_PREFILL = "/apps/ifolder3/account/prefill";
  public const string KEY_IFOLDER_ACCOUNT_SERVER_ADDRESS = "/apps/ifolder3/account/server_address";
  public const string KEY_IFOLDER_ACCOUNT_USER_NAME = "/apps/ifolder3/account/user_name";
  public const string KEY_IFOLDER_ACCOUNT_PASSWORD = "/apps/ifolder3/account/clear_text_password_for_testing_only";
  public const string KEY_IFOLDER_ACCOUNT_REMEMBER_PASSWORD= "/apps/ifolder3/account/remember_password";




  public const string KEY_IFOLDER_DEBUG_COLOR_PALETTE = "/apps/ifolder3/debug/color_palette";
  public const string KEY_IFOLDER_DEBUG_IFOLDER_DATA = "/apps/ifolder3/debug/ifolder_data";
  public const string KEY_IFOLDER_DEBUG_PRINT_SIMIAS_EVENT_ERRORS = "/apps/ifolder3/debug/print_simias_event_errors";

  private static GConf.Client client = null;
  private static GConf.NotifyEventHandler SettingChangedHandler;




  public static GConf.Client Client
  {
   get
   {
    if (client == null)
    {
     client = new GConf.Client();

     SettingChangedHandler =
      new GConf.NotifyEventHandler(OnSettingChanged);
     client.AddNotify("/apps/ifolder3", SettingChangedHandler);
    }

    return client;
   }
  }




  public static event GConf.NotifyEventHandler SettingChanged;






  static ClientConfig()
  {
  }



  private static object GetDefault(string key)
  {
   switch(key)
   {
    case KEY_SHOW_CREATION:
    case KEY_NOTIFY_IFOLDERS:
    case KEY_NOTIFY_COLLISIONS:
    case KEY_NOTIFY_USERS:

    case KEY_SHOW_SERVER_IFOLDERS:
    case KEY_IFOLDER_WINDOW_VISIBLE:

     return true;

    case KEY_IFOLDER_WINDOW_HIDE:
    case KEY_SHOW_SYNC_LOG:
    case KEY_NOTIFY_POLICY_VOILATION:
    case KEY_SHOW_QUOTA_VIOLATION:
    case KEY_SHOW_FILE_SIZE_VOILATION:
    case KEY_SHOW_EXCLUSION_VOILATION:
    case KEY_SHOW_DISK_FULL:
    case KEY_SHOW_PERMISSION_UNAVAILABLE:
    case KEY_SHOW_EXCEEDS_PATH_SIZE:
     return false;

    case KEY_SYNC_UNIT:
     return "Minutes";

    case KEY_IFOLDER_WINDOW_X_POS:
    case KEY_IFOLDER_WINDOW_Y_POS:
     return 0;
    case KEY_IFOLDER_WINDOW_WIDTH:
     return 640;
    case KEY_IFOLDER_WINDOW_HEIGHT:
     return 480;

    case KEY_IFOLDER_ACCOUNT_SERVER_ADDRESS:
    case KEY_IFOLDER_ACCOUNT_USER_NAME:
    case KEY_IFOLDER_ACCOUNT_PASSWORD:
     return "";

    case KEY_IFOLDER_ACCOUNT_PREFILL:
    case KEY_IFOLDER_ACCOUNT_REMEMBER_PASSWORD:
     return false;

    case KEY_IFOLDER_DEBUG_COLOR_PALETTE:
    case KEY_IFOLDER_DEBUG_IFOLDER_DATA:
    case KEY_IFOLDER_DEBUG_PRINT_SIMIAS_EVENT_ERRORS:
     return false;
   }

   return null;
  }

  private static void OnSettingChanged(object sender, GConf.NotifyEventArgs args)
  {
   if (SettingChanged != null)
    SettingChanged(sender, args);
  }



  public static object Get(string key)
  {
   try
   {
    return Client.Get(key);
   }
   catch (GConf.NoSuchKeyException)
   {
    object defaultValue = GetDefault(key);

    if (defaultValue != null)
     Client.Set(key, defaultValue);

    return defaultValue;
   }
  }

  public static void Set(string key, object value)
  {
   Client.Set(key, value);
  }






  public static bool Exists(string key)
  {
   try
   {
    Client.Get(key);
   }
   catch (GConf.NoSuchKeyException)
   {
    return false;
   }

   return true;
  }





  public static void DeleteKey(string key)
  {
   try
   {
    Client.Set(key, null);
   }
   catch{}
  }


 }
}
