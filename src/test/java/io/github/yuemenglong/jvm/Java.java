/**
 * Created by <yuemenglong@126.com> on 2018/2/8.
 */
package io.github.yuemenglong.jvm;

import scala.collection.mutable.HashTable;

import java.util.HashMap;
import java.util.Hashtable;

public class Java {

    public static double sub(double a, double b) {
        return a - b;
    }

    public static void main(String args[]) {
        int a = 1;
        double d = 10 + a;
        sub(d, 2);
    }
}
