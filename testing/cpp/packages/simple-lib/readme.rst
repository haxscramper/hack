To install this package into your local remote do

.. code-block:: bash
    conan install . --install-folder install
    conan build . --build-folder build --install-folder install --source-folder .
    conan package . --build-folder build --install-folder install
    conan create --test-build-folder=build . demo/testing
    conan export .
    conan upload simple-lib/0.1@demo/testing -r=local --all
