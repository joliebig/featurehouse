// ****************************************************************************
// 
// Copyright (C) 2005-2009  Doom9 & al
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// 
// ****************************************************************************

using System;
using System.Collections.Generic;
using System.Text;

namespace MeGUI.core.util
{
    public static class Tuple
    {
        public static Tuple<A, B, C, D, E, F> Create<A, B, C, D, E, F>(A a, B b, C c, D d, E e, F f)
        {
            return new Tuple<A, B, C, D, E, F>(a, b, c, d, e, f);
        }
        public static Tuple<A, B, C, D, E> Create<A, B, C, D, E>(A a, B b, C c, D d, E e)
        {
            return new Tuple<A, B, C, D, E>(a, b, c, d, e);
        }
        public static Tuple<A, B, C, D> Create<A, B, C, D>(A a, B b, C c, D d)
        {
            return new Tuple<A, B, C, D>(a, b, c, d);
        }
        public static Tuple<A, B, C> Create<A, B, C>(A a, B b, C c)
        {
            return new Tuple<A, B, C>(a, b, c);
        }
        public static Tuple<A, B> Create<A, B>(A a, B b)
        {
            return new Tuple<A, B>(a, b);
        }
    }

    public class Tuple<A, B>
    {
        public A a;
        public B b;

        public Tuple(A a, B b)
        {
            this.a = a;
            this.b = b;
        }
        public void get(out A _a, out B _b)
        {
            _a = a;
            _b = b;
        }

    }

    public class Tuple<A, B, C>
    {
        public A a;
        public B b;
        public C c;

        public Tuple(A a, B b, C c)
        {
            this.a = a;
            this.b = b;
            this.c = c;
        }
        public void get(out A _a, out B _b, out C _c)
        {
            _a = a;
            _b = b;
            _c = c;
        }
    }
    public class Tuple<A, B, C, D>
    {
        public A a;
        public B b;
        public C c;
        public D d;

        public Tuple(A a, B b, C c, D d)
        {
            this.a = a;
            this.b = b;
            this.c = c;
            this.d = d;
        }
        public void get(out A _a, out B _b, out C _c, out D _d)
        {
            _a = a;
            _b = b;
            _c = c;
            _d = d;
        }
    }

    public class Tuple<A, B, C, D, E>
    {
        public A a;
        public B b;
        public C c;
        public D d;
        public E e;

        public Tuple(A a, B b, C c, D d, E e)
        {
            this.a = a;
            this.b = b;
            this.c = c;
            this.d = d;
            this.e = e;
        }

        public void get(out A _a, out B _b, out C _c, out D _d, out E _e)
        {
            _a = a;
            _b = b;
            _c = c;
            _d = d;
            _e = e;
        }
    }
    public class Tuple<A, B, C, D, E, F>
    {
        public A a;
        public B b;
        public C c;
        public D d;
        public E e;
        public F f;


        public Tuple(A a, B b, C c, D d, E e, F f)
        {
            this.a = a;
            this.b = b;
            this.c = c;
            this.d = d;
            this.e = e;
            this.f = f;
        }

        public void get(out A _a, out B _b, out C _c, out D _d, out E _e, out F _f)
        {
            _a = a;
            _b = b;
            _c = c;
            _d = d;
            _e = e;
            _f = f;
        }
    }
}
