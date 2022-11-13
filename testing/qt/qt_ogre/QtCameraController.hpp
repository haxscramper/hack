#ifndef QTCAMERACONTROLLER_H
#define QTCAMERACONTROLLER_H

#include <OgreCamera.h>
#include <OgreFrameListener.h>
#include <OgreSceneNode.h>
#include <OgreSceneManager.h>

#include <QKeyEvent>
#include <QMouseEvent>
#include <QObject>

// enum CameraStyle should be in other namespace than
// OgreBites::CameraStyle
namespace OgreQtBites {
enum CameraStyle // enumerator values for different styles of camera
                 // movement
{
    CS_FREELOOK,
    CS_ORBIT,
    CS_MANUAL
};

class QtCameraController : public QObject
{
    Q_OBJECT
  public:
    QtCameraController(Ogre::Camera* cam, Ogre::SceneNode* camNode);

    /// Swaps the camera on our camera man for another camera.
    inline void setCamera(Ogre::Camera* cam) { camera = cam; }

    inline Ogre::Camera* getCamera() { return camera; }

    /// Sets the target we will revolve around. Only applies for orbit
    /// style.
    inline void setTarget(Ogre::SceneNode* inTarget);

    inline Ogre::SceneNode* getTarget() { return target; }

    /// Sets the spatial offset from the target. Only applies for orbit
    /// style.
    void setYawPitchDist(
        Ogre::Radian yaw,
        Ogre::Radian pitch,
        Ogre::Real   dist);

    /// Sets the camera's top speed. Only applies for free-look style.
    inline void setTopSpeed(Ogre::Real topSpeed) {
        config.topSpeed = topSpeed;
    }

    inline Ogre::Real getTopSpeed() { return config.topSpeed; }

    /// Sets the movement style of our camera man.
    void setStyle(CameraStyle inStyle);

    inline CameraStyle getStyle() { return style; }

    /// Manually stops the camera when in free-look mode.
    void manualStop();

    bool frameRenderingQueued(const Ogre::FrameEvent& evt);

    /// Processes key presses for free-look style movement.
    void injectKeyDown(const QKeyEvent& evt);

    /// Processes key releases for free-look style movement.
    void injectKeyUp(const QKeyEvent& evt);

    /// Processes mouse movement differently for each style.
    void injectMouseMove(int relX, int relY);

    /// Processes mouse movement differently for each style.
    void injectWheelMove(const QWheelEvent& evt);

    /// Processes mouse presses. Only applies for orbit style.
    /// Left button is for orbiting, and right button is for zooming.
    void injectMouseDown(const QMouseEvent& evt);

    /// Processes mouse releases. Only applies for orbit style.
    /// Left button is for orbiting, and right button is for zooming.
    void injectMouseUp(const QMouseEvent& evt);

  signals:
    void cameraPositionChanged(float x, float y, float z);

  public slots:
    void setCameraPosition(float x, float y, float z);

  public:
    void translateCamera(
        const Ogre::Vector3&       pos,
        Ogre::Node::TransformSpace relativeTo = Ogre::Node::TS_PARENT);

    /// Movement control parameters
    struct MoveControlConfig
    {
        float acceleration = 0.5; /// Acceleration speed when 'forward' is
                                  /// pressed
        int deceleration = 10;    /// Deceleration speed when 'backward' is
                                  /// pressed
        int topSpeed     = 150;   /// Max speed in regular movement mode
        int topFastSpeed = 150 * 10; /// Max speed in 'fast' movement mode
    } config;

  protected:
    Ogre::Camera*    camera;
    Ogre::SceneNode* cameraNode;
    CameraStyle      style;
    Ogre::SceneNode* target;
    Ogre::Vector3    velocity;


    struct MoveControlState
    {
        bool goingForward = false;
        bool goingBack    = false;
        bool goingLeft    = false;
        bool goingRight   = false;
        bool goingUp      = false;
        bool goingDown    = false;
        bool fastMove     = false;
        bool zooming      = false;
        bool orbiting     = false;
    } controlState;
};
} // namespace OgreQtBites

#endif // QTCAMERACONTROLLER_H
