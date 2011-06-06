using System;
using System.Collections;
using DBus;
namespace NetworkManager
{
    public enum DeviceType {
        Unknown,
        Wired,
        Wireless
    }
    [Interface("org.freedesktop.NetworkManager.Devices")]
    internal abstract class DeviceProxy
    {
        [Method] public abstract string getName();
        [Method] public abstract uint getMode();
        [Method] public abstract int getType();
        [Method] public abstract string getHalUdi();
        [Method] public abstract uint getIP4Address();
        [Method] public abstract string getHWAddress();
        [Method] public abstract bool getLinkActive();
        [Method] public abstract NetworkProxy getActiveNetwork();
        [Method] public abstract NetworkProxy [] getNetworks();
    }
    public class Device : IEnumerable
    {
        private DeviceProxy device;
        internal Device(DeviceProxy device)
        {
            this.device = device;
        }
        public override string ToString()
        {
            System.Text.StringBuilder builder = new System.Text.StringBuilder();
            builder.Append("Name:             " + Name + "\n");
            builder.Append("Type:             " + Type + "\n");
            builder.Append("Mode:             " + Mode + "\n");
            builder.Append("HAL UDI:          " + HalUdi + "\n");
            builder.Append("IP4 Address:      " + IP4Address + "\n");
            builder.Append("Hardware Address: " + HardwareAddress + "\n");
            builder.Append("Link Active:      " + (IsLinkActive ? "Yes" : "No") + "\n");
            builder.Append("Networks: \n");
            int i = 0;
            foreach(Network network in Networks) {
                builder.Append("  [" + (i++) + "] Name:      " + network.Name + "\n");
                builder.Append("      Active:    " + (network.Equals(ActiveNetwork) ? "Yes" : "No") + "\n");
                builder.Append("      Strength:  " + network.Strength + "\n");
                builder.Append("      Frequency: " + network.Frequency + "\n");
                builder.Append("      Rate:      " + network.Rate + "\n");
                builder.Append("      Encrypted: " + (network.IsEncrypted ? "Yes" : "No") + "\n");
                builder.Append("      Mode:      " + network.Mode + "\n");
            }
            if(i == 0) {
                builder.Append("  (none)\n");
            }
            return builder.ToString();
        }
        public string Name {
            get {
                return device.getName();
            }
        }
        public DeviceType Type {
            get {
                switch(device.getType()) {
                    case 1: return DeviceType.Wired;
                    case 2: return DeviceType.Wireless;
                    default: return DeviceType.Unknown;
                }
            }
        }
        public uint Mode {
            get {
                return device.getMode();
            }
        }
        public string HalUdi {
            get {
                return device.getHalUdi();
            }
        }
        public System.Net.IPAddress IP4Address {
            get {
                return new System.Net.IPAddress(device.getIP4Address());
            }
        }
        public string HardwareAddress {
            get {
                return device.getHWAddress();
            }
        }
        public bool IsLinkActive {
            get {
                return device.getLinkActive();
            }
        }
        public Network ActiveNetwork {
            get {
                if(Type != DeviceType.Wireless) {
                    return null;
                }
                try {
                    return new Network(device.getActiveNetwork());
                } catch(DBusException) {
                    return null;
                }
            }
        }
        public IEnumerator GetEnumerator()
        {
            foreach(NetworkProxy network in device.getNetworks()) {
                yield return new Network(network);
            }
        }
        public Network [] Networks {
            get {
                ArrayList list = new ArrayList();
                try {
                    foreach(NetworkProxy network in device.getNetworks()) {
                        list.Add(new Network(network));
                    }
                } catch(Exception) {
                }
                return list.ToArray(typeof(Network)) as Network [];
            }
        }
    }
}
