
// Copy of https://docs.oracle.com/javase/7/docs/api/javax/management/NotificationListener.html

class Notification {};

interface NotificationListener {

  void handleNotification(Notification notification, Object handback);   

}
