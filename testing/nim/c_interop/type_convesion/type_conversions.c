typedef void (*CallbackT)(void);

CallbackT callback;

__attribute__((visibility("default"))) void setCallback(CallbackT arg) {
    callback = arg;
}

__attribute__((visibility("default"))) void invokeCallback() {
    callback();
}
