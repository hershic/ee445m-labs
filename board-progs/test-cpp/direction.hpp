#ifndef __directionpp__
#define __directionpp__

/*! \addtogroup Direction
 * @{
 */

enum Direction { FORWARD, BACKWARD, LEFT, RIGHT, };

class nav {
public:
    static Direction opposite(Direction d) {
        switch(d) {
        case FORWARD: return BACKWARD;
        case BACKWARD: return FORWARD;
        case LEFT: return RIGHT;
        case RIGHT: return LEFT;
        }
    }
};

/*! End doxygen group
 * @}
 */

#endif
