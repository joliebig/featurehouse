




using System;
using System.Reflection;
using System.Collections;
using DBus;

namespace NetworkManager
{
    public enum State {
        Unknown = 0,
        Asleep,
        Connecting,
        Connected,
        Disconnected
    }

    [Interface("org.freedesktop.NetworkManager")]
    internal abstract class ManagerProxy
    {


        [Method] public abstract DeviceProxy [] getDevices();
        [Method] public abstract uint state();
        [Method] public abstract void sleep();
        [Method] public abstract void wake();
        [Method] public abstract bool getWirelessEnabled();
        [Method] public abstract void setWirelessEnabled(bool enabled);
        [Method] public abstract DeviceProxy getActiveDevice();
    }

    public class Manager : IEnumerable, IDisposable
    {
        private static readonly string PATH_NAME = "/org/freedesktop/NetworkManager";
        private static readonly string INTERFACE_NAME = "org.freedesktop.NetworkManager";

        private Service dbus_service;
        private Connection dbus_connection;
        private ManagerProxy manager;


        public event EventHandler DeviceNoLongerActive;
        public event EventHandler DeviceNowActive;
        public event EventHandler DeviceActivating;
        public event EventHandler DevicesChanged;
        public event EventHandler DeviceActivationStage;
        public event EventHandler DeviceIP4AddressChange;
        public event EventHandler StateChange;
        public event EventHandler WirelessNetworkDisappeared;
        public event EventHandler WirelessNetworkAppeared;
        public event EventHandler WirelessNetworkStrengthChanged;


        public Manager()
        {
            dbus_connection = Bus.GetSystemBus();
            dbus_service = Service.Get(dbus_connection, INTERFACE_NAME);
            manager = (ManagerProxy)dbus_service.GetObject(typeof(ManagerProxy), PATH_NAME);

            dbus_service.SignalCalled += OnSignalCalled;
        }

        public void Dispose()
        {

            System.GC.SuppressFinalize(manager);
        }

        private void OnSignalCalled(Signal signal)
        {
            if(signal.PathName != PATH_NAME || signal.InterfaceName != INTERFACE_NAME) {
                return;
            }

            InvokeEvent(signal.Name);
        }

        private void InvokeEvent(string nmSignalName)
        {


            switch(nmSignalName) {
                case "DeviceNoLongerActive": InvokeEvent(DeviceNoLongerActive); break;
                case "DeviceNowActive": InvokeEvent(DeviceNowActive); break;
                case "DeviceActivating": InvokeEvent(DeviceActivating); break;
                case "DeviceActivationStage": InvokeEvent(DeviceActivationStage); break;
                case "DevicesChanged": InvokeEvent(DevicesChanged); break;
                case "DeviceIP4AddressChange": InvokeEvent(DeviceIP4AddressChange); break;
                case "WirelessNetworkDisappeared": InvokeEvent(WirelessNetworkDisappeared); break;
                case "WirelessNetworkAppeared": InvokeEvent(WirelessNetworkAppeared); break;
                case "WirelessNetworkStrengthChanged": InvokeEvent(WirelessNetworkStrengthChanged); break;
                case "StateChange": InvokeEvent(StateChange); break;
            }


        }

        private void InvokeEvent(EventHandler eventHandler)
        {
            EventHandler handler = eventHandler;
            if(handler != null) {
                handler(this, new EventArgs());
            }
        }

        public IEnumerator GetEnumerator()
        {
            foreach(DeviceProxy device in manager.getDevices()) {
                yield return new Device(device);
            }
        }

        public State State {
            get {
                return (State)manager.state();
            }
        }

        public Device [] Devices {
            get {
                ArrayList list = new ArrayList();

                foreach(DeviceProxy device in manager.getDevices()) {
                    list.Add(new Device(device));
                }

                return list.ToArray(typeof(Device)) as Device [];
            }
        }

        public void Wake()
        {
            manager.wake();
        }

        public void Sleep()
        {
            manager.sleep();
        }

        public bool WirelessEnabled {
            get {
                return manager.getWirelessEnabled();
            }

            set {
                manager.setWirelessEnabled(value);
            }
        }

        public Device ActiveDevice {
            get {
                foreach(Device device in this) {
                    if(device.IsLinkActive) {
                        return device;
                    }
                }

                return null;
            }
        }
    }
}
