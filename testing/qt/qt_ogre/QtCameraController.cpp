#include <QDebug>

#include "QtCameraController.hpp"

OgreQtBites::QtCameraController::QtCameraController(
    Ogre::Camera*    cam,
    Ogre::SceneNode* camNode)
    : camera(0), target(0), velocity(Ogre::Vector3::ZERO) {


    qDebug() << "Created controller";

    setCamera(cam);
    cameraNode = camNode;
    setStyle(CS_FREELOOK);
}

void OgreQtBites::QtCameraController::setTarget(
    Ogre::SceneNode* inTarget) {
    if (inTarget != target) {
        target = inTarget;
        if (inTarget) {
            setYawPitchDist(Ogre::Degree(0), Ogre::Degree(15), 150);
            cameraNode->setAutoTracking(true, target);
        } else {
            cameraNode->setAutoTracking(false);
        }
    }
}

void OgreQtBites::QtCameraController::setYawPitchDist(
    Ogre::Radian yaw,
    Ogre::Radian pitch,
    Ogre::Real   dist) {
    cameraNode->setPosition(target->_getDerivedPosition());
    cameraNode->setOrientation(target->_getDerivedOrientation());
    cameraNode->yaw(yaw);
    cameraNode->pitch(-pitch);
    translateCamera(Ogre::Vector3(0, 0, dist), Ogre::Node::TS_LOCAL);
}

void OgreQtBites::QtCameraController::setStyle(CameraStyle inStyle) {
    if (style != CS_ORBIT && inStyle == CS_ORBIT) {
        setTarget(
            target ? target
                   : camera->getSceneManager()->getRootSceneNode());
        cameraNode->setFixedYawAxis(true);
        manualStop();
        setYawPitchDist(Ogre::Degree(0), Ogre::Degree(15), 150);
    } else if (style != CS_FREELOOK && inStyle == CS_FREELOOK) {
        cameraNode->setAutoTracking(false);
        cameraNode->setFixedYawAxis(true);
    } else if (style != CS_MANUAL && inStyle == CS_MANUAL) {
        cameraNode->setAutoTracking(false);
        manualStop();
    }
    style = inStyle;
}

void OgreQtBites::QtCameraController::manualStop() {
    if (style == CS_FREELOOK) {
        controlState.goingForward = false;
        controlState.goingBack    = false;
        controlState.goingLeft    = false;
        controlState.goingRight   = false;
        controlState.goingUp      = false;
        controlState.goingDown    = false;
        velocity                  = Ogre::Vector3::ZERO;
    }
}

bool OgreQtBites::QtCameraController::frameRenderingQueued(
    const Ogre::FrameEvent& evt) {
    if (style == CS_FREELOOK) {
        // build our acceleration vector based on keyboard input
        // composite
        Ogre::Vector3 accel = Ogre::Vector3::ZERO;
        if (controlState.goingForward) {
            accel += cameraNode->getOrientation().zAxis() * -1;
        }
        if (controlState.goingBack) {
            accel -= cameraNode->getOrientation().zAxis() * -1;
        }
        if (controlState.goingRight) {
            accel += cameraNode->getOrientation().xAxis();
        }
        if (controlState.goingLeft) {
            accel -= cameraNode->getOrientation().xAxis();
        }
        if (controlState.goingUp) {
            accel += cameraNode->getOrientation().yAxis();
        }
        if (controlState.goingDown) {
            accel -= cameraNode->getOrientation().yAxis();
        }

        // if accelerating, try to reach top speed in a certain time
        Ogre::Real topSpeed = controlState.fastMove ? config.topFastSpeed
                                                    : config.topSpeed;

        if (accel.squaredLength() != 0) {
            accel.normalise();
            velocity += accel * topSpeed * evt.timeSinceLastFrame
                        * config.acceleration;
        }
        // if not accelerating, try to stop in a certain time
        else {
            velocity -= velocity * evt.timeSinceLastFrame
                        * config.deceleration;
        }

        Ogre::Real tooSmall = std::numeric_limits<Ogre::Real>::epsilon();

        // keep camera velocity below top speed and above epsilon
        if (velocity.squaredLength() > topSpeed * topSpeed) {
            velocity.normalise();
            velocity *= topSpeed;
        } else if (velocity.squaredLength() < tooSmall * tooSmall) {
            velocity = Ogre::Vector3::ZERO;
        }

        if (velocity != Ogre::Vector3::ZERO) {
            translateCamera(velocity * evt.timeSinceLastFrame);
        }
    }

    return true;
}

void OgreQtBites::QtCameraController::injectKeyDown(const QKeyEvent& evt) {
    if (style == CS_FREELOOK) {
        if (evt.key() == Qt::Key_W || evt.key() == Qt::Key_Up) {
            controlState.goingForward = true;

        } else if (evt.key() == Qt::Key_S || evt.key() == Qt::Key_Down) {
            controlState.goingBack = true;

        } else if (evt.key() == Qt::Key_A || evt.key() == Qt::Key_Left) {
            controlState.goingLeft = true;

        } else if (evt.key() == Qt::Key_D || evt.key() == Qt::Key_Right) {
            controlState.goingRight = true;

        } else if (evt.key() == Qt::Key_PageUp) {
            controlState.goingUp = true;

        } else if (evt.key() == Qt::Key_PageDown) {
            controlState.goingDown = true;

        } else if (evt.key() == Qt::Key_Shift) {
            controlState.fastMove = true;
        }
    }
}

void OgreQtBites::QtCameraController::injectKeyUp(const QKeyEvent& evt) {
    if (style == CS_FREELOOK) {
        if (evt.key() == Qt::Key_W || evt.key() == Qt::Key_Up) {
            controlState.goingForward = false;

        } else if (evt.key() == Qt::Key_S || evt.key() == Qt::Key_Down) {
            controlState.goingBack = false;

        } else if (evt.key() == Qt::Key_A || evt.key() == Qt::Key_Left) {
            controlState.goingLeft = false;

        } else if (evt.key() == Qt::Key_D || evt.key() == Qt::Key_Right) {
            controlState.goingRight = false;

        } else if (evt.key() == Qt::Key_PageUp) {
            controlState.goingUp = false;

        } else if (evt.key() == Qt::Key_PageDown) {
            controlState.goingDown = false;

        } else if (evt.key() == Qt::Key_Shift) {
            controlState.fastMove = false;
        }
    }
}

void OgreQtBites::QtCameraController::injectMouseMove(int relX, int relY) {
    if (style == CS_ORBIT) {
        Ogre::Real dist = (cameraNode->getPosition()
                           - target->_getDerivedPosition())
                              .length();

        if (controlState.orbiting) {
            // yaw around the target, and pitch locally
            cameraNode->setPosition(target->_getDerivedPosition());

            cameraNode->yaw(Ogre::Degree(-relX * 0.025f));
            cameraNode->pitch(Ogre::Degree(-relY * 0.025f));

            translateCamera(
                Ogre::Vector3(0, 0, dist), Ogre::Node::TS_LOCAL);

            // don't let the camera go over the top or around the
            // bottom of the target
        } else if (controlState.zooming) // move the camera toward or
                                         // away from the target
        {
            // the further the camera is, the faster it moves
            translateCamera(
                Ogre::Vector3(0, 0, relY * 0.004f * dist),
                Ogre::Node::TS_LOCAL);
        }
    } else if (style == CS_FREELOOK) {
        cameraNode->yaw(Ogre::Degree(-relX * 0.15f));
        cameraNode->pitch(Ogre::Degree(-relY * 0.15f));
    }
}

void OgreQtBites::QtCameraController::injectWheelMove(
    const QWheelEvent& evt) {
    int relZ = evt.delta();
    if (style == CS_ORBIT) {
        Ogre::Real dist = (cameraNode->getPosition()
                           - target->_getDerivedPosition())
                              .length();

        if (relZ != 0) {
            // move the camera toward or away from the target
            // the further the camera is, the faster it moves
            translateCamera(
                Ogre::Vector3(0, 0, -relZ * 0.0008f * dist),
                Ogre::Node::TS_LOCAL);
        }
    }
}

void OgreQtBites::QtCameraController::injectMouseDown(
    const QMouseEvent& evt) {
    if (style == CS_ORBIT) {
        if (evt.buttons() & Qt::LeftButton) {
            controlState.orbiting = true;
        } else if (evt.buttons() & Qt::RightButton) {
            controlState.zooming = true;
        }
    }
}

void OgreQtBites::QtCameraController::injectMouseUp(
    const QMouseEvent& evt) {
    if (style == CS_ORBIT) {
        if (evt.buttons() & Qt::LeftButton) {
            controlState.orbiting = false;
        } else if (evt.buttons() & Qt::RightButton) {
            controlState.zooming = false;
        }
    }
}

void OgreQtBites::QtCameraController::setCameraPosition(
    float x,
    float y,
    float z) {
    cameraNode->translate(Ogre::Vector3(x, y, z));
}

void OgreQtBites::QtCameraController::translateCamera(
    const Ogre::Vector3&       pos,
    Ogre::Node::TransformSpace relativeTo) {
    cameraNode->translate(pos, relativeTo);

    auto newpos = cameraNode->getPosition();
    emit cameraPositionChanged(newpos.x, newpos.y, newpos.z);
}
