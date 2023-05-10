#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>
#define RAND0N( N_MAX ) ((int)floor((N_MAX+1)*(double)rand()/RAND_MAX))

const int WIDTH=16000;
const int HEIGHT=9000;
enum{ BUST_RADIUS=1760,BUST_LIMIT=900,RELEASE_RADIUS=1600,VISION_RADIUS=2200,STUN_RADIUS=1760};
const int BUST_RADIUS2=BUST_RADIUS*BUST_RADIUS;
const int BUST_LIMIT2=BUST_LIMIT*BUST_LIMIT;
const int RELEASE_RADIUS2=RELEASE_RADIUS*RELEASE_RADIUS;
const int VISION_RADIUS2=VISION_RADIUS*VISION_RADIUS;
const int STUN_RADIUS2=STUN_RADIUS*STUN_RADIUS;


typedef struct{
    int x;
    int y;
} xy_s;

static inline xy_s XYSum(xy_s r1,xy_s r2)
{    return (xy_s){.x=r2.x+r1.x , .y=r2.y+r1.y }; };

static inline xy_s XYDiff(xy_s r1,xy_s r2)
{    return (xy_s){.x=r2.x-r1.x , .y=r2.y-r1.y }; };

static inline double XYNorm2( xy_s d )
{    return (d.x*d.x+d.y*d.y); };

typedef struct{
    int id; // buster id or ghost id
    xy_s pos; // position of this buster / ghost
    int type; // the team id if it is a buster, -1 if it is a ghost.
    int state; // For busters: 0=idle, 1=carrying a ghost. 0 by default and 1 if already booked by a buster
    int value; // For busters: Ghost id being carried. For ghosts: number of busters attempting to trap this ghost.
} entity_s;

int nMyBuster,nEnemyBuster,nGhost;
xy_s myBase, enemyBase,buffbase;
int stunReady[10]={0};
/**
 * Send your busters out into the fog to trap ghosts and bring them home!
 **/

int CanRelease( entity_s buster );

int CanBust( entity_s buster , entity_s ghost );

int CanSee( entity_s buster , entity_s ghost );

int CanOnlySee( entity_s buster , entity_s ghost );

int CanStun( entity_s buster , entity_s b2 );

int main()
{
    srand(time(NULL));
    int bustersPerPlayer; // the amount of busters you control
    scanf("%d", &bustersPerPlayer);
    int ghostCount; // the amount of ghosts on the map
    scanf("%d", &ghostCount);
    int myTeamId; // if this is 0, your base is on the top left of the map, if it is one, on the bottom right
    scanf("%d", &myTeamId);
    myBase.x=0;myBase.y=0;enemyBase.x=WIDTH;enemyBase.y=HEIGHT;
    if(myTeamId != 0 ){buffbase=myBase;myBase=enemyBase;enemyBase=buffbase;}

    entity_s myBuster[bustersPerPlayer],enemyBuster[bustersPerPlayer],ghost[ghostCount];
    // game loop
    while (1) {
        for( int i=0;i<10;i++) stunReady[i]+=1;
        nMyBuster=0;nEnemyBuster=0;nGhost=0;

        int entities; // the number of busters and ghosts visible to you
        scanf("%d", &entities);
        for (int i = 0; i < entities; i++) {
            entity_s entity;
            scanf("%d%d%d%d%d%d", &entity.id, &entity.pos.x, &entity.pos.y, &entity.type, &entity.state, &entity.value);

            if(entity.type== -1){
                ghost[nGhost]=entity;
                nGhost++;
            }else if(entity.type == myTeamId){
                myBuster[nMyBuster]=entity;
                nMyBuster++;
            }else{
                enemyBuster[nEnemyBuster]=entity;
                nEnemyBuster++;
            }
        }

        fprintf(stderr,"mybuster:\n");
        for (int i = 0; i < nMyBuster ; i++) fprintf(stderr,"%d %d %d %d %d %d\n",  myBuster[i].id,  myBuster[i].pos.x,  myBuster[i].pos.y,  myBuster[i].type,  myBuster[i].state,  myBuster[i].value);
        
        fprintf(stderr,"ghost:\n");
        for (int i = 0; i < nGhost ; i++) fprintf(stderr,"%d %d %d %d %d %d\n",  ghost[i].id,  ghost[i].pos.x,  ghost[i].pos.y,  ghost[i].type,  ghost[i].state,  ghost[i].value);


        for (int i = 0; i < nMyBuster; i++) {
            int ok=0;

            //try to release
            if(ok == 0){
                if( myBuster[i].state == 1 )
                {
                    if(CanRelease(myBuster[i])){
                        printf("RELEASE\n");
                        ok=1;
                    }
                }
            }

            //stun if possible
            if( ok == 0){
                if(stunReady[i]>0){
                    for(int j = 0; j < nEnemyBuster; j++){
                        if(CanStun(myBuster[i],enemyBuster[j]) && enemyBuster[i].state != 2 ){
                            printf("STUN %d\n",enemyBuster[j].id);
                            stunReady[i]=-20;
                            ok=1;
                            break;
                        }
                    }
                }
            }

            //if charged go back to base
            if(ok == 0){
                if( myBuster[i].state == 1 )
                {
                    printf("MOVE %d %d \n",myBase.x,myBase.y);
                    ok=1;
                }
            }

            //if can bust -> bust
            if(ok == 0){
                for(int j = 0; j < nGhost ; j++ ){
                    if(CanBust(myBuster[i],ghost[j]) && ghost[j].state == 0 ){
                        printf("BUST %d\n",ghost[j].id);
                        ghost[j].state=1;
                        ok=1;
                        break;
                    }
                }
            }

            //try to follow a visible ghost if out of range
            if(ok == 0){
                for(int j = 0; j < nGhost ; j++ ){
                    if(CanOnlySee(myBuster[i],ghost[j]) && ghost[j].state == 0 ){
                        printf("MOVE %d %d\n",ghost[j].pos.x,ghost[j].pos.y);
                        ok=1;
                        break;
                    }
                }
            }

            //go in a random direction
            if(ok == 0 ){

                int direction=RAND0N(2);
                switch (direction){ 
                    case 0:
                        printf("MOVE %d %d\n",enemyBase.x,enemyBase.y);
                        break;

                    case 1:
                        printf("MOVE %d %d\n",myBase.x,enemyBase.y);
                        break;

                    case 2:
                        printf("MOVE %d %d\n",enemyBase.x,myBase.y);
                        break;

                    default:
                        printf("MOVE %d %d\n",myBase.x,myBase.y);
                        break;
                }
            }
        }//end buster loop
    }//end game loop

    return 0;
}


int CanRelease( entity_s buster )
{
    if( XYNorm2(XYDiff(buster.pos,myBase)) < RELEASE_RADIUS2 )
        return 1;
    else
        return 0;
}

int CanBust( entity_s buster , entity_s ghost )
{
    int d= XYNorm2(XYDiff(buster.pos,ghost.pos));
    if(d < BUST_RADIUS2 && d > BUST_LIMIT2 )
        return 1;
    else
        return 0;
}

int CanSee( entity_s buster , entity_s ghost )
{
    int d= XYNorm2(XYDiff(buster.pos,ghost.pos));
    if(d < VISION_RADIUS2)
        return 1;
    else
        return 0;
}

int CanOnlySee( entity_s buster , entity_s ghost )
{
    int d= XYNorm2(XYDiff(buster.pos,ghost.pos));
    if(d < VISION_RADIUS2 && d > BUST_RADIUS )
        return 1;
    else
        return 0;
}

int CanStun( entity_s buster , entity_s b2 )
{
    int d= XYNorm2(XYDiff(buster.pos,b2.pos));
    if(d < STUN_RADIUS2 )
        return 1;
    else
        return 0;
}






