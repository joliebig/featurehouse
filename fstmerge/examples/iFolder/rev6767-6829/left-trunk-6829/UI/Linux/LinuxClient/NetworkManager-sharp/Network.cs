using System;
using DBus;
namespace NetworkManager
{
    [Interface("org.freedesktop.NetworkManager.Devices")]
    internal abstract class NetworkProxy
    {
        [Method] public abstract string getName();
        [Method] public abstract int getStrength();
        [Method] public abstract double getFrequency();
        [Method] public abstract int getRate();
        [Method] public abstract bool getEncrypted();
        [Method] public abstract uint getMode();
    }
    public class Network
    {
        private NetworkProxy network;
        internal Network(NetworkProxy network)
        {
            this.network = network;
        }
        public string Name {
            get {
                return network.getName();
            }
        }
        public int Strength {
            get {
                return network.getStrength();
            }
        }
        public double Frequency {
            get {
                return network.getFrequency();
            }
        }
        public int Rate {
            get {
                return network.getRate();
            }
        }
        public bool IsEncrypted {
            get {
                return network.getEncrypted();
            }
        }
        public uint Mode {
            get {
                return network.getMode();
            }
        }
        public override bool Equals(object o)
        {
            Network compare = o as Network;
            if(compare == null) {
                return false;
            }
            return Name == compare.Name;
        }
        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }
        public override string ToString()
        {
            return Name;
        }
    }
}
