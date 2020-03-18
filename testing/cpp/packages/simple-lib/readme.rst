To install this package into your local remote do

.. code-blocl:: bash
    conan install . --install-folder install  
    conan package . --build-folder build --install-folder install     
    conan package . --build-folder build --install-folder install
    conan create --test-build-folder=build . demo/testing   
    conan upload simple-lib/0.1@demo/testing -r=local --all
