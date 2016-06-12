var list = [];
var list2 = [];
for(var i = 0; i < 10; i = i + 1){
	if(i == 5)break;
	list = list.concat([i]);
	list2 = list2.concat([i+1]);
}
list.compare(list2);
