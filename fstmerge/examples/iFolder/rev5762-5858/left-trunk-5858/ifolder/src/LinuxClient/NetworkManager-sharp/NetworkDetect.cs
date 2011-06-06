using System;
using System.Collections;
using Mono.Unix;
namespace NetworkManager
{
    public delegate void NetworkStateChangedHandler(object o, NetworkStateChangedArgs args);
    public class NetworkStateChangedArgs : EventArgs
    {
        public bool Connected;
    }
    public class NetworkDetect : IDisposable
    {
        public event NetworkStateChangedHandler StateChanged;
        private Manager nm_manager;
        private State last_state;
        private static NetworkDetect instance;
        public static NetworkDetect Instance {
            get {
                if(instance == null) {
                    instance = new NetworkDetect();
                }
                return instance;
            }
        }
        public void Dispose()
        {
            if(nm_manager != null) {
                nm_manager.Dispose();
            }
        }
        private NetworkDetect()
        {
            try {
                ConnectToNetworkManager();
            } catch(Exception e) {
                nm_manager = null;
                Console.WriteLine("Cannot connect to NetworkManager");
                Console.WriteLine("An available, working network connection will be assumed");
            }
        }
        private void ConnectToNetworkManager()
        {
            nm_manager = new Manager();
            nm_manager.StateChange += OnNetworkManagerEvent;
            nm_manager.DeviceNowActive += OnNetworkManagerEvent;
            nm_manager.DeviceNoLongerActive += OnNetworkManagerEvent;
            last_state = nm_manager.State;
        }
        private void OnNetworkManagerEvent(object o, EventArgs args)
        {
            try {
                State new_state = nm_manager.State;
                if(new_state != last_state && (new_state == State.Connected || new_state == State.Disconnected)) {
                    last_state = new_state;
                    NetworkStateChangedHandler handler = StateChanged;
                    if(handler != null) {
                        NetworkStateChangedArgs state_changed_args = new NetworkStateChangedArgs();
                        state_changed_args.Connected = Connected;
                        handler(this, state_changed_args);
                    }
                    Device active_device = nm_manager.ActiveDevice;
                    if(Connected && active_device != null) {
                     Console.WriteLine("Network Connection Established: {0} ({1})",
                           active_device.Name, active_device.IP4Address);
                    } else if(Connected) {
                     Console.WriteLine("Network Connection Established: Active Device Unknown");
                    } else {
                     Console.WriteLine("Network Connection Unavailable: Disconnected");
                    }
                }
            } catch(Exception) {
            }
        }
        public bool Connected {
            get {
                try {
                    return nm_manager == null ? true : nm_manager.State == State.Connected;
                } catch(Exception) {
                    return true;
                }
            }
        }
        public Manager Manager {
            get {
                return nm_manager;
            }
        }
    }
}
