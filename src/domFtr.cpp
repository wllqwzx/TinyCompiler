#include <iostream>
#include <string.h>
#include <vector>
#include <queue>
#include <stack>
#include <map>
#include <set>
#include <algorithm>

using namespace std;

//CFG
int chds[105][105];
int fath[105][105];
bool acc[105];

//dom tree
int domChds[105][105];
int domFath[105];

//entry point
int start;

//get entry point
int getStart(){
	int deg[105];
	memset(deg,-1,sizeof(deg));
	int i,j;
	for(i=0;i<=100;i++){
		for(j=0;j<=100;j++){
			if(chds[i][j]!=0){
				if(deg[i] == -1){
					deg[i] = 0;
				}
				if(deg[j] == -1){
					deg[j] = 1;
				}else{
					deg[j] += 1;
				}

			}
		}
	}
	for(i=0;i<=100;i++){
		if(deg[i] == 0)
			return i;
	}
	return -1; // error!
}


int getLowestCommFath(queue<int> v){
	if(v.empty()){
		return -1;	//entry point
	}else{
		int root = v.front();
		v.pop();
		while(!v.empty()){
			int tt = v.front();
			v.pop();
			vector<int> froot;
			vector<int> ftt;

			froot.insert(froot.begin(), root);
			while(domFath[root] != -1){
				root = domFath[root];
				froot.insert(froot.begin(), root);
			}

			ftt.insert(ftt.begin(), tt);
			while(domFath[tt] != -1){
				tt = domFath[tt];
				ftt.insert(ftt.begin(), tt);
			}

			int i = 0;
			while(i<froot.size() && i<ftt.size() && froot[i] == ftt[i]){
				i++;
			}
			root = froot[i-1];
		}

		return root;
	}
}

//generate dom tree
void computeDomTree()
{
	start = getStart();
	queue<int> q;
	q.push(start);
	acc[start] = true;
	while(!q.empty()){
		int tt = q.front();
		q.pop();
		int i;
		queue<int> tfa;	//all fathers in tree 
		for(i=0;i<=100;i++){
			if(fath[tt][i] == 1 && acc[i] == true){
				tfa.push(i);
			}
		}

		int lowCommFa = getLowestCommFath(tfa);
		
		if(lowCommFa != -1){ //is not start point
			domChds[lowCommFa][tt] = 1;
			domFath[tt] = lowCommFa;
		}
		for(i=0;i<=100;i++){
			if(acc[i] == false && chds[tt][i] != 0){
				q.push(i);
				acc[i] = true;
			}
		}
	}
}

//========================================
//compute dom frontier


vector<int> df[105];

bool isDominate(int fa, int chi){
	while(domFath[chi]!=-1){
		if(domFath[chi] == fa){
			return true;
		}
		chi = domFath[chi];
	}
	return false;
}

void computeDf(int node){
	int i,j;
	for(i=0;i<=100;i++){
		if(chds[node][i] == 1 && domFath[i] != node){
			df[node].push_back(i);
		}
	}

	for(i=0;i<=100;i++){
		if(domChds[node][i] == 1){
			computeDf(i);
			vector<int>::iterator iter;
			for(iter = df[i].begin();iter != df[i].end();iter++){
				if(isDominate(node,*iter) == false){
					df[node].push_back(*iter);
				}
			}
		}
	}
}


int main()
{
	freopen(".temp.in", "r", stdin);
	freopen(".temp.out", "w", stdout);
	int f,t;
	memset(chds,0,sizeof(chds));
	memset(fath,0,sizeof(fath));
	memset(acc,0,sizeof(acc));
	memset(domFath,-1,sizeof(domFath));
	memset(domChds,0,sizeof(domChds));

	while(scanf("%d %d",&f,&t)!=EOF){
		chds[f][t] = 1;
		fath[t][f] = 1;
	}

	computeDomTree();

	//test dom tree
	/*
	printf("start:: %d\n", start);
	for(int i = 0;i<=100;i++){
		if(domFath[i] != -1){
			printf("%d --> %d\n", i,domFath[i]);
		}
	}
	*/

	
	computeDf(start);
	//ptint out dom friter
	for(int i=0;i<=100;i++){
		if(acc[i] == true){
			printf("%d ", i);
			printf("%d ", (int)df[i].size());
			for(vector<int>::iterator iter = df[i].begin(); iter != df[i].end(); iter++){
				printf("%d ", *iter);
			}
			printf("\n");
		}
	}

	return 0;
}
