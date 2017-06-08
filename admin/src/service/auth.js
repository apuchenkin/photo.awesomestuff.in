const AUTH = 'auth';

const getToken = () => localStorage.getItem(AUTH) || '';

const logout = () => {
  localStorage.removeItem(AUTH);

  return Promise.resolve();
};

export default {
  getToken,
  logout,
};
