from distutils.core import setup

setup(
  name = 'fclpy',
  packages = ['fclpy'],
  version = '0.0.1',
  license='MIT',
  description = 'Fusion Common Lisp for Python',
  author = 'Ralph Ritoch',
  author_email = 'rritoch@outlook.com',
  url = 'https://github.com/fclpy/fclpy',
#  download_url = 'https://github.com/fclpy/fclpy/archive/v_01.tar.gz',
  keywords = ['lisp', 'common', 'fclpy', 'common lisp'],
  install_requires=[
#          'validators',
#          'beautifulsoup4',
      ],
  classifiers=[
    'Development Status :: 3 - Alpha',      # Chose either "3 - Alpha", "4 - Beta" or "5 - Production/Stable" as the current state of your package

    'Intended Audience :: Developers',
    'Topic :: Software Development :: Build Tools',

    'License :: OSI Approved :: MIT License',

    'Programming Language :: Python :: 3',
    'Programming Language :: Python :: 3.4',
    'Programming Language :: Python :: 3.5',
    'Programming Language :: Python :: 3.6',
  ],
)