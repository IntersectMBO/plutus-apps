Plutus Tools Developer Guide
===============================

Plutus Tools are a collection of off-chain infrastructure resources. Plutus Tools are built 
for developers who are authoring web-based applications that implement smart contracts on the 
Cardano blockchain. 

The Plutus Tools Team supports the underlying resources that developers need who are 
writing full applications using Plutus in Haskell, including off-chain code. 
The term “off-chain code” refers to the part of a contract application’s code which 
runs off the chain, usually as a contract application. 

Because there are so many potential use cases for what you may choose to create, you are not 
confined to only one prescribed way of using Plutus Tools. Instead, you can access 
the entire tool box where you can find a collection of utilities and components. Pick and choose 
from the tools and examples that best suit your use case. 

The Plutus Platform
-------------------------------

The Plutus Tools Repo (https://github.com/input-output-hk/plutus-apps) contains 
the Plutus Platform, which consists of these components: 

* Libraries which implement the Plutus Application Framework
* A selection of end-to-end use cases written with the Plutus Application Framework
* The Plutus Playground, a web-based playground for learning and writing basic Plutus Applications

To read more about the Platform, see :ref:`what_is_the_plutus_platform`.
To get started using the Platform, see :ref:`plutus_getting_started`.


.. toctree::
   :caption: Explore Plutus
   :maxdepth: 2

   plutus/explanations/index
   plutus/tutorials/index
   plutus/howtos/index
   plutus/troubleshooting

.. toctree::
   :caption: Architecture design records
   :maxdepth: 1

   adr/index

.. toctree::
   :caption: Reference
   :maxdepth: 2

   reference/index
