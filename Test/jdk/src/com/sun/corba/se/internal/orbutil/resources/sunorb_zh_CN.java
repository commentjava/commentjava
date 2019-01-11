package com.sun.corba.se.internal.orbutil.resources;

import java.util.ListResourceBundle;

public final class sunorb_zh_CN extends ListResourceBundle {
    protected final Object[][] getContents() {
        return new Object[][] {
            { "bootstrap.exception", "\u5C06\u5C5E\u6027\u4FDD\u5B58\u5230\u6587\u4EF6{0}\u65F6\u53D1\u751F\u5F02\u5E38\uFF1A\u5F02\u5E38 {1}" },
            { "bootstrap.filenotfound", "\u6CA1\u6709\u627E\u5230\u6587\u4EF6{0}" },
            { "bootstrap.filenotreadable", "\u4E0D\u53EF\u8BFB\u53D6\u6587\u4EF6 {0} " },
            { "bootstrap.success", "\u5C06\u7AEF\u53E3\u8BBE\u7F6E\u4E3A{0}\u5E76\u4ECE{1}\u8BFB\u53D6\u670D\u52A1" },
            { "bootstrap.usage", "\u7528\u6CD5\uFF1A{0} <\u9009\u9879> \n\n\u5176\u4E2D\uFF0C<\u9009\u9879> \u5305\u62EC\uFF1A\n  -ORBInitialPort        \u521D\u59CB\u7AEF\u53E3\uFF08\u5FC5\u9700\uFF09\n  -InitialServicesFile   \u5305\u542B\u521D\u59CB\u670D\u52A1\u5217\u8868\u7684\u6587\u4EF6\uFF08\u5FC5\u9700\uFF09\n" },
            { "orbd.commfailure", "\n\u7531\u4E8E ORBinitialPort \u5DF2\u5728\u4F7F\u7528\u4E2D\uFF0C\u542F\u52A8 ORBD \u5931\u8D25" },
            { "orbd.internalexception", "\n\u5185\u90E8\u5F02\u5E38\uFF0C\u542F\u52A8 ORBD \u5931\u8D25\u3002 \n\u53EF\u80FD\u539F\u56E0\uFF1A\n1. \u6307\u5B9A\u7684 ORBInitialPort \u6216 ORBActivationPort \u5DF2\u5728\u4F7F\u7528\u4E2D \n2. \u6CA1\u6709\u5199 orb.db \u7684\u5199\u5165\u6743\u9650 " },
            { "orbd.usage", "\u7528\u6CD5\uFF1A{0} <\u9009\u9879> \n\n\u5176\u4E2D\uFF0C<\u9009\u9879> \u5305\u62EC\uFF1A\n  -port                  \u6FC0\u6D3B\u542F\u52A8 ORBD \u7684\u7AEF\u53E3\uFF0C\u7F3A\u7701\u503C\u4E3A 1049 (\u53EF\u9009)\n  -defaultdb             ORBD \u6587\u4EF6\u7684\u76EE\u5F55\uFF0C\u7F3A\u7701\u503C\u4E3A \"./orb.db\" (\u53EF\u9009)\n  -serverid              ORBD \u7684\u670D\u52A1\u5668\u6807\u8BC6\u7B26\uFF0C\u7F3A\u7701\u503C\u4E3A 1 (\u53EF\u9009)\n  -ORBInitialPort        \u521D\u59CB\u7AEF\u53E3\uFF08\u5FC5\u9700\uFF09\n  -ORBInitialHost        \u521D\u59CB\u4E3B\u673A\u540D\u79F0\uFF08\u5FC5\u9700\uFF09\n" },
            { "pnameserv.success", "\u6301\u4E45\u6027\u540D\u79F0\u670D\u52A1\u5668\u6210\u529F\u542F\u52A8" },
            { "servertool.appname", "\tapplicationName     - {0}" },
            { "servertool.args", "\targs      - {0}" },
            { "servertool.baddef", "\u9519\u8BEF\u7684\u670D\u52A1\u5668\u5B9A\u4E49\uFF1A {0}" },
            { "servertool.banner", "\n\n\u6B22\u8FCE\u4F7F\u7528 Java IDL \u670D\u52A1\u5668\u5DE5\u5177 \n\u8BF7\u5728\u63D0\u793A\u5904\u8F93\u5165\u547D\u4EE4 \n" },
            { "servertool.classpath", "\tclasspath - {0}" },
            { "servertool.getserverid", "\n\tgetserverid [ -applicationName <name> ] \n" },
            { "servertool.getserverid1", "\u8FD4\u56DE\u5E94\u7528\u7A0B\u5E8F\u540D\u79F0\u7684\u670D\u52A1\u5668\u6807\u8BC6\u7B26" },
            { "servertool.getserverid2", "\t\u5E94\u7528\u7A0B\u5E8F\u540D\u79F0 {0} \u7684\u670D\u52A1\u5668\u6807\u8BC6\u7B26\u662F {1}" },
            { "servertool.helddown", "\t\u670D\u52A1\u5668\u5DF2\u88AB\u5173\u95ED\u3002" },
            { "servertool.help", "\thelp\n\t\u6216\n\thelp <command name>\n" },
            { "servertool.help1", "\u53D6\u5F97\u5E2E\u52A9" },
            { "servertool.list", "\n\t\u5217\u8868\n" },
            { "servertool.list1", "\u5217\u51FA\u6240\u6709\u5DF2\u6CE8\u518C\u670D\u52A1\u5668" },
            { "servertool.list2", "\n\t\u670D\u52A1\u5668\u6807\u8BC6\u7B26\t\t\u670D\u52A1\u5668\u7C7B\u540D\u79F0\t\t\t\u670D\u52A1\u5668\u5E94\u7528\u7A0B\u5E8F\n\t---------\t-----------------\t------------------\n" },
            { "servertool.listactive", "\n\tlistactive" },
            { "servertool.listactive1", "\u5217\u51FA\u5F53\u524D\u6D3B\u52A8\u7684\u670D\u52A1\u5668" },
            { "servertool.listappnames", "\tlistappnames\n" },
            { "servertool.listappnames1", "\u5217\u51FA\u5F53\u524D\u5B9A\u4E49\u7684\u5E94\u7528\u7A0B\u5E8F\u540D\u79F0 " },
            { "servertool.listappnames2", "\u5F53\u524D\u5B9A\u4E49\u7684\u670D\u52A1\u5668\u5E94\u7528\u7A0B\u5E8F\u540D\u79F0\uFF1A" },
            { "servertool.locate", "\n\tlocate [ -serverid <server id> | -applicationName <name> ] [ <-endpointType <endpointType> ] \n" },
            { "servertool.locate1", "\u4E3A\u5DF2\u6CE8\u518C\u670D\u52A1\u5668\u5B9A\u4F4D\u7279\u5B9A\u7C7B\u578B\u7684\u7AEF\u53E3" },
            { "servertool.locate2", "\n\n\t\u4E3B\u673A\u540D\u79F0 {0} \n\n\t\t\u7AEF\u53E3\t\t\u7AEF\u53E3\u7C7B\u578B\t\tORB \u6807\u8BC6\n\t\t----\t\t---------\t\t------\n" },
            { "servertool.locateorb", "\n\tlocateperorb [ -serverid <server id> | -applicationName <name> ] [ -orbid <ORB name> ]\n" },
            { "servertool.locateorb1", "\u4E3A\u5DF2\u6CE8\u518C\u670D\u52A1\u5668\u7684\u7279\u5B9A\u5BF9\u8C61\u8BF7\u6C42\u4EE3\u7406\u7A0B\u5E8F\u5B9A\u4F4D\u7AEF\u53E3\u3002" },
            { "servertool.locateorb2", "\n\n\t\u4E3B\u673A\u540D\u79F0 {0} \n\n\t\t\u7AEF\u53E3\t\t\u7AEF\u53E3\u7C7B\u578B\t\tORB \u6807\u8BC6\n\t\t----\t\t--------\t\t------\n" },
            { "servertool.name", "\tname      - {0}" },
            { "servertool.nosuchorb", "\t\u65E0\u6548\u7684\u5BF9\u8C61\u8BF7\u6C42\u4EE3\u7406\u7A0B\u5E8F (ORB)\u3002" },
            { "servertool.nosuchserver", "\t\u627E\u4E0D\u5230\u8FD9\u79CD\u670D\u52A1\u5668\u3002" },
            { "servertool.orbidmap", "\t\u7528\u6CD5\uFF1Aorblist [ -serverid <server id> | -applicationName <name> ]\n" },
            { "servertool.orbidmap1", "\u5BF9\u8C61\u8BF7\u6C42\u4EE3\u7406\u7A0B\u5E8F (orb) \u540D\u79F0\u53CA\u5176\u6620\u5C04\u5217\u8868" },
            { "servertool.orbidmap2", "\n\tORB \u6807\u8BC6\t\tORB \u540D\u79F0\n\t------\t\t--------\n" },
            { "servertool.quit", "\n\t\u9000\u51FA\n" },
            { "servertool.quit1", "\u9000\u51FA\u6B64\u5DE5\u5177" },
            { "servertool.register", "\n\n\tregister -server<\u670D\u52A1\u5668\u7C7B\u540D\u79F0> \n\t         -applicationName <\u5907\u7528\u670D\u52A1\u5668\u540D\u79F0> \n\t         -classpath <\u5230\u670D\u52A1\u5668\u7684\u7C7B\u8DEF\u5F84> \n\t         -args <\u670D\u52A1\u5668\u7684\u53C2\u6570> \n\t         -vmargs <\u670D\u52A1\u5668 Java VM \u7684\u53C2\u6570>\n" },
            { "servertool.register1", "\u6CE8\u518C\u4E00\u4E2A\u53EF\u6FC0\u6D3B\u7684\u670D\u52A1\u5668" },
            { "servertool.register2", "\t\u5DF2\u6CE8\u518C\u670D\u52A1\u5668 (serverid = {0})\u3002" },
            { "servertool.register3", "\t\u5DF2\u6CE8\u518C\u670D\u52A1\u5668\uFF0C\u4F46\u5DF2\u88AB\u5173\u95ED (serverid = {0})\u3002" },
            { "servertool.register4", "\t\u670D\u52A1\u5668\u5DF2\u6CE8\u518C (serverid = {0})\u3002" },
            { "servertool.serverid", "\t\u670D\u52A1\u5668\u6807\u8BC6\u7B26 - {0}" },
            { "servertool.servernotrunning", "\t\u670D\u52A1\u5668\u6CA1\u6709\u8FD0\u884C\u3002" },
            { "servertool.serverup", "\t\u670D\u52A1\u5668\u5DF2\u5F00\u542F\u3002" },
            { "servertool.shorthelp", "\n\n\t\u53EF\u7528\u547D\u4EE4\uFF1A\n\t------------------- \n" },
            { "servertool.shutdown", "\n\tshutdown [ -serverid <server id> | -applicationName <name> ]\n" },
            { "servertool.shutdown1", "\u5173\u95ED\u4E00\u4E2A\u5DF2\u6CE8\u518C\u670D\u52A1\u5668" },
            { "servertool.shutdown2", "\t\u670D\u52A1\u5668\u6210\u529F\u5173\u95ED\u3002" },
            { "servertool.startserver", "\n\tstartup [ -serverid <server id> | -applicationName <name> ]\n" },
            { "servertool.startserver1", "\u542F\u52A8\u4E00\u4E2A\u5DF2\u6CE8\u518C\u670D\u52A1\u5668" },
            { "servertool.startserver2", "\t\u670D\u52A1\u5668\u6210\u529F\u542F\u52A8\u3002" },
            { "servertool.unregister", "\n\tunregister [ -serverid <server id> | -applicationName <name> ] \n" },
            { "servertool.unregister1", "\u53D6\u6D88\u670D\u52A1\u5668\u6CE8\u518C" },
            { "servertool.unregister2", "\t\u670D\u52A1\u5668\u672A\u6CE8\u518C\u3002" },
            { "servertool.usage", "\u7528\u6CD5\uFF1A {0} <\u9009\u9879> \n\n\u5176\u4E2D\uFF0C<\u9009\u9879> \u5305\u62EC\uFF1A\n  -ORBInitialPort        \u521D\u59CB\u7AEF\u53E3\uFF08\u5FC5\u9700\uFF09\n  -ORBInitialHost        \u521D\u59CB\u4E3B\u673A\u540D\u79F0\uFF08\u5FC5\u9700\uFF09\n" },
            { "servertool.vmargs", "\tvmargs    - {0}" },
            { "tnameserv.exception", "\u542F\u52A8{0}\u7AEF\u53E3\u4E0A\u7684\u81EA\u5F15\u5BFC\u7A0B\u5E8F\u670D\u52A1\u65F6\u53D1\u751F\u5F02\u5E38" },
            { "tnameserv.hs1", "\u521D\u59CB\u7684\u547D\u540D\u8303\u56F4\uFF1A\n{0}" },
            { "tnameserv.hs2", "TransientNameServer: \u5C06\u521D\u59CB\u5BF9\u8C61\u5F15\u7528\u7AEF\u53E3\u8BBE\u7F6E\u4E3A\uFF1A{0}" },
            { "tnameserv.hs3", "\u51C6\u5907\u5C31\u7EEA\u3002" },
            { "tnameserv.invalidhostoption", "ORBInitialHost \u4E0D\u662F\u540D\u79F0\u670D\u52A1\u5668\u7684\u6709\u6548\u9009\u9879" },
            { "tnameserv.orbinitialport0", "ORBInitialPort 0 \u4E0D\u662F\u540D\u79F0\u670D\u52A1\u5668\u7684\u6709\u6548\u9009\u9879" },
            { "tnameserv.usage", "\u5C1D\u8BD5\u5229\u7528\u547D\u4EE4\u884C\u53C2\u6570\u4F7F\u7528\u4E0D\u540C\u7684\u7AEF\u53E3 -ORBInnitialPort <portno>" },
        };
    }
}
