import React from 'react';
import { NavLink } from 'react-router-dom';

export default () => (
  <header>
    <nav>
      <ul>
        <li><NavLink to={'/'}>Home</NavLink></li>
        <li><NavLink to={'/upload'}>Upload</NavLink></li>
      </ul>
    </nav>
  </header>
);
