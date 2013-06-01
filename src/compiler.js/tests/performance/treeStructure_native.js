"use strict"

var ctimeBegin$1 = function($_var1) {
    return console.time($_var1);
};
var ctimeEnd$2 = function($_var5) {
    return console.timeEnd($_var5);
};
var repeat$3 = function($_var9) {
    return function($_var10) {
        return function($_var11) {
            var $_var13 = function($_var14) {
                return (function() {
                    switch ($_var14) {
                        case 0:
                            return $_var10($_var11)
                        default:
                            return ($_var10($_var11), $_var13($_var14 - 1))
                    }
                }());
            };
            return $_var13($_var9 - 1);
        };
    };
};
var timerep$4 = function($_var35) {
    return function($_var36) {
        return function($_var37) {
            return repeat$3($_var35)(function($_var41) {
                return (ctimeBegin$1($_var37), ($_var36($_var41), ctimeEnd$2($_var37)));
            });
        };
    };
};

var Tree = function(val, left, right){
    var instance = {};
    instance.val = val;
    instance.leftTree = left;
    instance.rightTree = right;
    instance.tag = "Tree";
    return instance;
}
var Leaf = function(){
    var instance = {};
    instance.tag = "Leaf";
    return instance;
}

var intList = function(n) {
    var list = [];
    while (n >= 0) {
        list.push(n)
        --n;
    }
    return list;
}

var rev = function(list) {
    var revlist = [];
    var n = list.length;
    while (n-- > 0) {
        revlist.push(list[n]);
    }
    return revlist;
}

var genTreeMake = function(n) {
    var nums = intList(19);
    return genTree(nums);
}

var genTree = function(nums) {
    switch(nums.length) {
        case 0:
            return Leaf();
        default:
            var head = nums.shift();
            var revlist = rev(nums)
            return Tree(head, genTree(nums), genTree(revlist));
    }
}

var traverseTree = function(tree, n) {
    switch (tree.tag) {
        case "Tree":
            return (n == tree.val ? 1 : 0) + traverseTree(tree.leftTree, n) + traverseTree(tree.rightTree, n);
        default:
            return 0;
    }
}

var tree = genTree(intList(19));
var $_var154 = timerep$4(100)(genTreeMake)("genTree_native")(19);
var $_var171 = timerep$4(100)(traverseTree)("traverseTree_native")(tree, 0);