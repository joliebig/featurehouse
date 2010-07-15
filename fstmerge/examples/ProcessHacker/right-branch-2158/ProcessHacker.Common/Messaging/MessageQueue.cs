using System;
using System.Collections.Generic;
using System.Text;

namespace ProcessHacker.Common.Messaging
{
    public class MessageQueue
    {
        private Queue<Message> _queue = new Queue<Message>();
        private List<MessageQueueListener> _listeners = new List<MessageQueueListener>();

        public MessageQueue()
        {

            this.AddListener(new MessageQueueListener<ActionMessage>((action) => action.Action()));
        }

        public void AddListener(MessageQueueListener listener)
        {
            lock (_listeners)
                _listeners.Add(listener);
        }

        public void Enqueue(Message message)
        {
            lock (_queue)
                _queue.Enqueue(message);
        }

        public void EnqueueAction(Action action)
        {
            this.Enqueue(new ActionMessage(action));
        }

        public void Listen()
        {
            lock (_queue)
            {

                while (_queue.Count > 0)
                {
                    Message message = _queue.Dequeue();


                    lock (_listeners)
                    {
                        foreach (MessageQueueListener listener in _listeners)
                        {

                            if (listener.Type.IsInstanceOfType(message))
                                listener.Callback(message);
                        }
                    }
                }
            }
        }

        public void RemoveListener(MessageQueueListener listener)
        {
            lock (_listeners)
                _listeners.Remove(listener);
        }
    }
}
