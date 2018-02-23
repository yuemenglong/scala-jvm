/**
 * Created by <yuemenglong@126.com> on 2018/2/8.
 */
package io.github.yuemenglong.jvm;

import java.util.*;

public class Java {

    private Set entrySet;

    public Set entrySet() {
        if (entrySet == null)
            entrySet = new HashSet<>();
        return entrySet;
    }

    public static void main(String args[]) {
        Properties p = new Properties();
        Set<Map.Entry<Object, Object>> s = p.entrySet();
    }

    @Override
    public String toString() {
        return "JavaToString";
    }
}
